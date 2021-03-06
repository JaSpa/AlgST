{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module AlgST.Rename.Operators (rewriteOperatorSequence) where

import AlgST.Builtins.Names qualified as B
import AlgST.Rename.Monad
import AlgST.Rename.Phase
import AlgST.Syntax.Expression qualified as E
import AlgST.Syntax.Kind qualified as K
import AlgST.Syntax.Name
import AlgST.Syntax.Operators
import AlgST.Util.ErrorMessage
import Control.Category ((>>>))
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Validate
import Data.DList.DNonEmpty qualified as DNE
import Data.Foldable
import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty (NonEmpty (..))

data OpGrouping op = OpGrouping
  { leadingExpr :: Maybe RnExp,
    opExprPairs :: [(op, RnExp)],
    trailingOp :: Maybe op
  }
  deriving stock (Show, Functor, Foldable, Traversable)

data ResolvedOp = ResolvedOp
  { opLoc :: Pos,
    opName :: NameR Values,
    opExpr :: RnExp,
    opPrec :: Precedence,
    opAssoc :: Associativity
  }

instance HasPos ResolvedOp where
  pos = opLoc

instance ErrorMsg ResolvedOp where
  msg = opName >>> msg
  msgStyling = opName >>> msgStyling

noTypeArgs :: ResolvedOp -> Bool
noTypeArgs (opExpr -> E.Var {}) = True
noTypeArgs _ = False

rewriteOperatorSequence :: OperatorSequence Rn -> RnM RnExp
rewriteOperatorSequence =
  groupOperators
    >>> traverse resolveOperator
    >=> foldGroupedOperators

groupOperators :: OperatorSequence Rn -> OpGrouping RnExp
groupOperators = \case
  OperandFirst rs (e :| es) ->
    OpGrouping
      { leadingExpr = Just e,
        opExprPairs = pairs es,
        trailingOp = rs
      }
  OperatorFirst rs ne ->
    OpGrouping
      { leadingExpr = Nothing,
        opExprPairs = pairs (toList ne),
        trailingOp = rs
      }
  where
    pairs (a : b : xs) = (a, b) : pairs xs
    pairs _ = []

extractOperator :: RnExp -> Maybe (Pos, NameR Values)
extractOperator (E.Var p v) = Just (p, v)
extractOperator (E.TypeApp _ e _) = extractOperator e
extractOperator _ = Nothing

resolveOperator :: MonadValidate DErrors m => RnExp -> m ResolvedOp
resolveOperator e = do
  (opLoc, opName) <-
    refuteNothing invalidOpErr $
      extractOperator e
  (prec, assoc) <-
    refuteNothing (unknownOpErr opLoc opName) $
      HM.lookup opName B.builtinOperators
  pure
    ResolvedOp
      { opLoc = opLoc,
        opName = opName,
        opExpr = e,
        opPrec = prec,
        opAssoc = assoc
      }
  where
    refuteNothing e =
      maybe (refute (DNE.singleton e)) pure
    invalidOpErr =
      PosError (pos e) [Error "Invalid operator", Error e]
    unknownOpErr loc name =
      PosError loc [Error "Unknown operator", Error name]

data Prec = MinPrec | Prec !Precedence | MaxPrec
  deriving (Eq, Ord, Show)

instance Bounded Prec where
  minBound = MinPrec
  maxBound = MaxPrec

foldGroupedOperators :: OpGrouping ResolvedOp -> RnM RnExp
foldGroupedOperators = \case
  OpGrouping Nothing [] _ ->
    -- The parser handles a single operator. See the 'EOps' production.
    error "internal parsing error: operator sequence without operands"
  OpGrouping Nothing ((op, _) : _) _ ->
    refute $ DNE.singleton $ errorUnsupportedRightSection op
  OpGrouping (Just e) ops mopr ->
    foldOperators e ops mopr

foldOperators :: RnExp -> [(ResolvedOp, RnExp)] -> Maybe ResolvedOp -> RnM RnExp
foldOperators e0 ops0 = \case
  Nothing ->
    -- Ordinary operator chain.
    --
    -- `go` will always consume all operator-operand pairs because every
    -- operator has a higher precedence than `minBound`. Therefore we can
    -- discard the second component.
    fst <$> go e0 minBound Nothing ops0
  Just secOp -> do
    -- Operator section.
    --
    -- We use the operator to the right as the starting precedence. Should `go`
    -- not consume all operator-operand pairs we emit an error. Such a section
    -- could be for example
    --
    --    (3 + 4 *)
    --
    -- We want to prohibit those. Although a possible desugaring would be
    --
    --    (*) (3+4)
    --
    -- this breaks very easily when adding the second argument for (*),
    -- leaving us with
    --
    --    (+) 3 ((*) 4 x)
    --
    -- which suddenly associates differently.
    (e, remainingOps) <- go e0 (nextPrec Left secOp) (Just secOp) ops0
    case remainingOps of
      [] ->
        -- All fine. Construct the final partial application.
        buildOpApplication secOp e Nothing
      (op, _) : _ ->
        refute . pure $ errorPrecConflict secOp op
  where
    -- The 'Side' specifies wether parsing continues on the left or the right
    -- of the operator.
    --
    -- If the operator associates for example to the left and parsing continues
    -- on the left we do not increase the precedence context.
    nextPrec :: Side -> ResolvedOp -> Prec
    nextPrec side op
      | opAssoc op /= NA && opAssoc op /= select side L R =
        if opPrec op == maxBound
          then MaxPrec
          else Prec $ succ $ opPrec op
      | otherwise =
        Prec $ opPrec op

    go ::
      RnExp ->
      Prec ->
      Maybe ResolvedOp ->
      [(ResolvedOp, RnExp)] ->
      RnM (RnExp, [(ResolvedOp, RnExp)])
    go lhs minPrec mprev ((op, rhs) : ops)
      | Just prevOp <- mprev,
        minPrec == Prec (opPrec op) && opAssoc prevOp == NA =
        refute $ DNE.singleton $ errorNonAssocOperators prevOp op
      | minPrec <= Prec (opPrec op) = do
        (rhs', ops') <- go rhs (nextPrec Right op) (Just op) ops
        res <- buildOpApplication op lhs (Just rhs')
        go res minPrec (Just op) ops'
    go lhs _ _ ops =
      pure (lhs, ops)

buildOpApplication :: ResolvedOp -> RnExp -> Maybe RnExp -> RnM RnExp
buildOpApplication op lhs mrhs
  | -- (<|)
    opName op == B.opPipeBwd && noTypeArgs op,
    Just rhs <- mrhs =
    -- Desugar operator to direct function application.
    pure $ E.App (pos op) lhs rhs
  | -- (|>)
    opName op == B.opPipeFwd && noTypeArgs op,
    Just rhs <- mrhs =
    -- Desugar operator to (flipped) direct function application.
    pure $ E.App (pos op) rhs lhs
  | -- (<&>)
    opName op == B.opMapAfter && noTypeArgs op,
    Just rhs <- mrhs = lift . lift $ do
    -- Desugar to
    --    let (a, c) = lhs in (rhs a, c)
    let loc = pos op
    a <- freshResolvedU $ Unqualified "a"
    c <- freshResolvedU $ Unqualified "c"
    pure
      . E.PatLet loc (op @- B.conPair) [op @- a, op @- c] lhs
      $ E.Pair loc (E.App loc rhs (E.Var loc a)) (E.Var loc c)
  | -- (<*>)
    opName op == B.opAppComb && noTypeArgs op,
    Just rhs <- mrhs = lift . lift $ do
    -- Desugar to:
    -- \c1 -> let (f, c2) = lhs c1 in let (x, c3) = rhs c2 in (f x, c3)
    let po = pos op
    c1 <- freshResolvedU $ Unqualified "c1"
    c2 <- freshResolvedU $ Unqualified "c2"
    c3 <- freshResolvedU $ Unqualified "c3"
    f <- freshResolvedU $ Unqualified "f"
    x <- freshResolvedU $ Unqualified "x"
    pure
      . E.Abs po
      . E.Bind po K.Lin c1 Nothing
      . E.PatLet po (op @- B.conPair) [op @- f, op @- c2] (E.App po lhs (E.Var po c1))
      . E.PatLet po (op @- B.conPair) [op @- x, op @- c3] (E.App po rhs (E.Var po c2))
      $ E.Pair po (E.App po (E.Var po f) (E.Var po x)) (E.Var po c3)
  | otherwise = do
    let appLhs = E.App (pos lhs) (opExpr op) lhs
    pure $ maybe appLhs (E.App <$> pos <*> pure appLhs <*> id) mrhs

type Side = forall a. a -> Either a a

select :: Side -> b -> b -> b
select side = either const (const id) . side

errorUnsupportedRightSection :: ResolvedOp -> Diagnostic
errorUnsupportedRightSection op =
  -- The reason we can't support right sections yet: we would have to generate
  -- a lambda abstraction which requires for us to know the type we have to
  -- give the parameter. At the current stage we don't know this type yet.
  PosError
    (pos op)
    [ Error "Operator",
      Error op,
      Error "is missing its right operand.",
      ErrLine,
      Error "(Right sections are not yet supported.)"
    ]
{-# NOINLINE errorUnsupportedRightSection #-}

errorNonAssocOperators :: ResolvedOp -> ResolvedOp -> Diagnostic
errorNonAssocOperators v1 v2 =
  PosError
    (pos v2)
    [ Error "Non-associative operators",
      ErrLine,
      Error "   ",
      Error v1,
      Error "at",
      Error (pos v1),
      ErrLine,
      Error "and",
      Error v2,
      Error "at",
      Error (pos v2),
      ErrLine,
      Error "are used next to each other.",
      ErrLine,
      ErrLine,
      Error "Use parentheses to explicitly specify the associativity."
    ]
{-# NOINLINE errorNonAssocOperators #-}

errorPrecConflict :: ResolvedOp -> ResolvedOp -> Diagnostic
errorPrecConflict secOp otherOp =
  PosError
    (pos secOp)
    [ Error "The operator",
      Error secOp,
      Error "of a section must have lower precedence",
      ErrLine,
      Error "        than",
      Error otherOp,
      Error "at",
      Error $ pos otherOp,
      ErrLine,
      Error "Use parentheses to explicitly specify the associativity."
    ]
{-# NOINLINE errorPrecConflict #-}
