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
import AlgST.Util.Diagnose qualified as D
import AlgST.Util.SourceLocation
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Validate
import Data.CallStack
import Data.Foldable
import Data.Function
import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty (NonEmpty (..))
import Data.These.Combinators

data OpGrouping op = OpGrouping
  { leadingExpr :: Maybe RnExp,
    opExprPairs :: [(op, RnExp)],
    trailingOp :: Maybe op,
    opsRange :: SrcRange
  }
  deriving stock (Show, Functor, Foldable, Traversable)

data ResolvedOp = ResolvedOp
  { opName :: NameR Values,
    opExpr :: RnExp,
    opPrec :: Precedence,
    opAssoc :: Associativity
  }

instance HasRange ResolvedOp where
  getRange = getRange . opExpr

type Unresolved = RnExp

type UnresolvedSeq h = OperatorSequence h Rn Unresolved

type ResolvedSeq h = OperatorSequence h Rn ResolvedOp

noTypeArgs :: ResolvedOp -> Bool
noTypeArgs (opExpr -> E.Var {}) = True
noTypeArgs _ = False

operatorBaseName :: HasCallStack => RnExp -> RnName Values
operatorBaseName (E.Var _ n) = n
operatorBaseName (E.TypeApp _ e _) = operatorBaseName e
operatorBaseName e = error $ "internal error: expression is not an operator ‘" ++ show e ++ "’"

rewriteOperatorSequence :: UnresolvedSeq h -> RnM RnExp
rewriteOperatorSequence = traverse resolveOperator >=> foldGroupedOperators

resolveOperator :: D.MonadErrors m => Unresolved -> m ResolvedOp
resolveOperator op = do
  let name = operatorBaseName op
  (prec, assoc) <-
    D.failNothing unknownOpErr $
      HM.lookup name B.builtinOperators
  pure
    ResolvedOp
      { opName = name,
        opExpr = op,
        opPrec = prec,
        opAssoc = assoc
      }
  where
    unknownOpErr =
      D.err
        (getRange op)
        "unknown operator"
        "operator has no associativity/precedence information"

data Prec = MinPrec | Prec !Precedence | MaxPrec
  deriving (Eq, Ord, Show)

instance Bounded Prec where
  minBound = MinPrec
  maxBound = MaxPrec

foldGroupedOperators :: ResolvedSeq h -> RnM RnExp
foldGroupedOperators (OpSeqO _ ((op, _) :| _) _) =
  -- The reason being that we don't know the types involved to do a proper
  -- desugaring into a lambda abstraction.
  --
  -- TODO: Either include a proper AST node for the typechecker to resolve or
  -- move operator folding into the typechecking stage.
  D.fatalError $ errorUnsupportedRightSection op
foldGroupedOperators (OpSeqE r e tail) =
  foldOperators r e (maybe [] toList (justHere tail)) (justThere tail)

foldOperators :: SrcRange -> RnExp -> [(ResolvedOp, RnExp)] -> Maybe ResolvedOp -> RnM RnExp
foldOperators sequenceRange e0 ops0 = \case
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
    -- We use the operator to the right as the starting precedence. We emit an
    -- error should `go` not consume all operator-operand pairs. Such a section
    -- could be, for example,
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
        refute . pure $ errorPrecConflict sequenceRange secOp op
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
          D.fatalError $ errorNonAssocOperators prevOp op
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
      pure $ E.App (getRange op) lhs rhs
  | -- (|>)
    opName op == B.opPipeFwd && noTypeArgs op,
    Just rhs <- mrhs =
      -- Desugar operator to (flipped) direct function application.
      pure $ E.App (getRange op) rhs lhs
  | -- (<&>)
    opName op == B.opMapAfter && noTypeArgs op,
    Just rhs <- mrhs = lift . lift $ do
      -- Desugar to
      --    let (a, c) = lhs in (rhs a, c)
      let loc = getRange op
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
      let po = getRange op
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
      let appLhs = E.App (getRange lhs) (opExpr op) lhs
      pure $ maybe appLhs (E.App <$> getRange <*> pure appLhs <*> id) mrhs

type Side = forall a. a -> Either a a

select :: Side -> b -> b -> b
select side = either const (const id) . side

errorUnsupportedRightSection :: ResolvedOp -> D.Diagnostic
errorUnsupportedRightSection op =
  D.err
    (getRange op)
    "unsupported right section"
    "operator is missing its left operand"
    & D.note "Right sections are not yet supported."
    & D.hint "Write an explicit lambda abstraction."
{-# NOINLINE errorUnsupportedRightSection #-}

errorNonAssocOperators :: ResolvedOp -> ResolvedOp -> D.Diagnostic
errorNonAssocOperators v1 v2 =
  -- TODO: can we include the outer arguments in the range?
  D.err
    (v1 `runion` v2)
    "associativity conflict"
    "non-associative operators with equal precedence are used next to each other"
    & D.context (getRange v1 `min` getRange v2) "operator #1"
    & D.context (getRange v1 `max` getRange v2) "operator #2"
    & parensHint
{-# NOINLINE errorNonAssocOperators #-}

errorPrecConflict :: SrcRange -> ResolvedOp -> ResolvedOp -> D.Diagnostic
errorPrecConflict sectionRange secOp otherOp =
  D.err
    sectionRange
    "precedence conflict"
    "the operator of a section must have lower predence than its operand"
    & D.context (getRange otherOp) "this operator has higher precedence"
    & D.context (getRange secOp) "than this section operator"
    & parensHint
{-# NOINLINE errorPrecConflict #-}

parensHint :: D.Diagnostic -> D.Diagnostic
parensHint = D.hint "Use parentheses to explicitly specify the associativity."
