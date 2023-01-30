{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AlgST.Typing.Error where

import AlgST.Parse.Unparser
import AlgST.Rename
import AlgST.Syntax.Decl
import AlgST.Syntax.Expression qualified as E
import AlgST.Syntax.Kind qualified as K
import AlgST.Syntax.Name
import AlgST.Syntax.Pos
import AlgST.Syntax.Type qualified as T
import AlgST.Typing.Align
import AlgST.Typing.Monad
import AlgST.Typing.Phase
import AlgST.Util
import AlgST.Util.ErrorMessage hiding (Errors)
import AlgST.Util.SourceLocation (HasRange, needPLoc, needPos, needRange)
import Control.Monad.Validate
import Data.DList.DNonEmpty qualified as DNE
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Singletons
import Data.These
import Data.Void
import Prelude hiding (truncate)

add :: MonadValidate Errors m => Diagnostic -> m ()
add !e = dispute $ This $ DNE.singleton e

-- | Records multiple errors. If the error list is acutally empty, no errors
-- will be recorded and the computation won't fail.
adds :: MonadValidate Errors m => [Diagnostic] -> m ()
adds = maybe (pure ()) (dispute . This . DNE.fromNonEmpty) . nonEmpty

fatal :: MonadValidate Errors m => Diagnostic -> m a
fatal !e = refute $ This $ DNE.singleton e

ifNothing :: MonadValidate Errors m => Diagnostic -> Maybe a -> m a
ifNothing e = maybe (fatal e) pure

unexpectedKind ::
  (T.ForallX HasRange x) =>
  T.Type x ->
  K.Kind ->
  [K.Kind] ->
  Diagnostic
unexpectedKind t kind hintKinds = PosError (needPos t) (message ++ hint)
  where
    message =
      [ Error "Unexpected kind",
        Error kind,
        Error "for type",
        Error "‹‹show t››"
      ]
    hint = case nonEmpty hintKinds of
      Just hints ->
        [ErrLine, Error $ "Expected a subkind of " ++ joinOr (show <$> hints)]
      Nothing ->
        []
{-# NOINLINE unexpectedKind #-}

unexpectedForkKind :: String -> RnExp -> TcType -> K.Kind -> K.Kind -> Diagnostic
unexpectedForkKind forkKind e ty kiActual kiExpected =
  PosError (pos e) $
    errUnline
      [ [Error $ "Forked expression (" ++ forkKind ++ ")"],
        [indent, Error e],
        [Error "has type"],
        showType ty Nothing,
        [ Error "which has kind",
          Error kiActual,
          Error "but should have kind",
          Error kiExpected
        ]
      ]
{-# NOINLINE unexpectedForkKind #-}

typeMismatch :: RnExp -> TcType -> TcType -> TcType -> TcType -> Diagnostic
typeMismatch expr tyActual tyActualNF tyExpected tyExpectedNF =
  PosError (pos expr) $
    errUnline
      [ [Error "Expression"],
        [indent, Error expr],
        [Error "has type"],
        showType tyActual (Just tyActualNF),
        [Error "but context demands a subtype of"],
        showType tyExpected (Just tyExpectedNF)
      ]
{-# NOINLINE typeMismatch #-}

typeMismatchBind :: K.Bind stage a -> TcType -> RnExp -> Diagnostic
typeMismatchBind (K.Bind p v _ _) t e =
  PosError (needPos p) $
    errUnline
      [ [Error "Binding of type variable", Error v, Error "in expression"],
        [indent, Error e],
        [Error "does not align with type"],
        showType t Nothing
      ]
{-# NOINLINE typeMismatchBind #-}

-- It is unlikely that this error can be triggered. But I feel that it is
-- better to have an error message at hand should it be needed than crashing
-- the compiler.
noNormalform :: TcType -> Diagnostic
noNormalform t = PosError (needPos t) [Error "Malformed type:", Error t]
{-# NOINLINE noNormalform #-}

missingUse :: ProgVar TcStage -> Var -> Diagnostic
missingUse name var =
  PosError (pos var) . errUnline $
    [ [Error "Linear variable", Error name, Error "of type"],
      showType (varType var) Nothing,
      [Error "is unused."]
    ]
{-# NOINLINE missingUse #-}

invalidConsumed :: Pos -> ProgVar TcStage -> Var -> Pos -> Diagnostic
invalidConsumed contextLoc name var loc =
  PosError
    loc
    [ Error "Linear variable",
      Error name,
      ErrLine,
      Error "   bound at",
      Error (varLocation var),
      ErrLine,
      Error "Consumed in unrestricted context",
      ErrLine,
      Error "   started at",
      Error contextLoc
    ]
{-# NOINLINE invalidConsumed #-}

linVarUsedTwice :: Pos -> Pos -> ProgVar TcStage -> Var -> Diagnostic
linVarUsedTwice loc1 loc2 name var =
  PosError
    (max loc1 loc2)
    [ Error "Linear variable",
      Error name,
      Error "is used twice.",
      ErrLine,
      Error "Bound at:",
      Error (varLocation var),
      ErrLine,
      Error " Used at:",
      Error (min loc1 loc2),
      ErrLine,
      Error "         ",
      Error (max loc1 loc2)
    ]
{-# NOINLINE linVarUsedTwice #-}

noArrowType :: RnExp -> TcType -> Diagnostic
noArrowType e t =
  PosError
    (pos e)
    [ Error "Type of",
      ErrLine,
      Error "  ",
      Error e,
      ErrLine,
      Error "is neither a function type nor convertible to a function type:",
      ErrLine,
      Error "  ",
      Error t
    ]
{-# NOINLINE noArrowType #-}

noForallType :: RnExp -> TcType -> Diagnostic
noForallType e t =
  PosError
    (pos e)
    [ Error "Type of",
      ErrLine,
      Error "  ",
      Error e,
      ErrLine,
      Error "is neither a forall type nor convertible to a forall type:",
      ErrLine,
      Error "  ",
      Error t
    ]
{-# NOINLINE noForallType #-}

typeConstructorNParams :: Pos -> NonEmpty RnType -> Int -> Int -> Diagnostic
typeConstructorNParams loc ts !given !expected =
  PosError
    loc
    [ Error "Invalid type application",
      ErrLine,
      Error "  ",
      Error $ foldl1 (T.App (needRange loc)) ts,
      ErrLine,
      Error "Type constructor",
      Error $ NE.head ts,
      Error "needs",
      Error expected,
      Error $ plural expected "parameter," "paramters,",
      Error given,
      Error "provided."
    ]
{-# NOINLINE typeConstructorNParams #-}

cyclicAliases :: [ExpansionEntry] -> Diagnostic
cyclicAliases aliases =
  PosError errLoc $
    Error "Cycle in type synonym declarations."
      : concat
        [ concat
            [ [ ErrLine,
                Error $ "  " ++ showLoc expansionLoc
              ],
              aliasHead expansionName (aliasParams expansionAlias),
              [ Error "=",
                Error (aliasType expansionAlias)
              ]
            ]
          | ExpansionEntry {..} <- aliases
        ]
  where
    errLoc = minimum positions
    locSize = maximum (length . show <$> positions)
    positions = fmap expansionLoc aliases
    showLoc (show -> l) = l ++ ":" ++ replicate (locSize - length l) ' '
    aliasHead name params = Error "type" : Error name : [Error p | _ :@ (p, _) <- fmap needPLoc params]
{-# NOINLINE cyclicAliases #-}

invalidNominalKind :: Pos -> String -> TypeVar TcStage -> K.Kind -> NonEmpty K.Kind -> Diagnostic
invalidNominalKind loc nomKind name actual allowed =
  PosError
    loc
    [ Error "The declared type of",
      Error name,
      Error "is",
      Error actual,
      ErrLine,
      Error $ "‘" ++ nomKind ++ "’",
      Error "declarations can only declare types with",
      Error $ plural allowed "kind" "kinds",
      Error $ joinOr $ fmap show allowed,
      Error "(the default)."
    ]
{-# NOINLINE invalidNominalKind #-}

mismatchedBind :: Pos -> ANameG scope -> TcType -> Diagnostic
mismatchedBind loc var t =
  PosError loc $
    Error (choose "Binding of type variable" "Binding of variable")
      : Error var
      : Error "does not align with type"
      : ErrLine
      : showType t Nothing
  where
    choose x y = either (const x) (const y) var
{-# NOINLINE mismatchedBind #-}

invalidPatternExpr :: String -> Pos -> TcType -> TcType -> Diagnostic
invalidPatternExpr desc loc scrutTy tyNF =
  PosError loc $
    errUnline
      [ [Error desc, Error "has type"],
        showType scrutTy (Just tyNF),
        [Error "which is not allowed in pattern expressions."]
      ]
{-# NOINLINE invalidPatternExpr #-}

invalidSessionCaseBranch :: E.CaseBranch f Rn -> Diagnostic
invalidSessionCaseBranch branch = PosError (E.branchPos branch) [Error msg]
  where
    msg = "Branches of a receiving case must bind exactly one variable."
{-# NOINLINE invalidSessionCaseBranch #-}

mismatchedCaseConstructor :: Pos -> TcType -> ProgVar TcStage -> Diagnostic
mismatchedCaseConstructor loc ty con =
  PosError
    loc
    [ Error con,
      Error "is not a constructor for type",
      Error ty
    ]
{-# NOINLINE mismatchedCaseConstructor #-}

missingCaseBranches :: Pos -> [ProgVar TcStage] -> Diagnostic
missingCaseBranches loc branches =
  PosError loc $
    Error "Incomplete case. Missing"
      : Error (plural branches "branch:" "branches:")
      : missingBranches branches
{-# NOINLINE missingCaseBranches #-}

noSingularConstructorType :: Pos -> TcType -> [ProgVar TcStage] -> Diagnostic
noSingularConstructorType loc ty branches =
  PosError loc $
    Error "Values of type"
      : ErrLine
      : Error "  "
      : Error ty
      : Error "cannot appear as the right hand side of a let-pattern."
      : ErrLine
      : Error "Too many constructors. Unhandled "
      : Error (plural branches "constructor:" "constructors:")
      : missingBranches branches
{-# NOINLINE noSingularConstructorType #-}

missingBranches :: [ProgVar TcStage] -> [ErrorMessage]
missingBranches branches =
  concat
    [ [ErrLine, Error "  ", branch]
      | branch <- truncate 3 (Error "...") (Error <$> branches)
    ]

emptyCaseExpr :: Pos -> Diagnostic
emptyCaseExpr loc = PosError loc [Error "Empty case expression."]
{-# NOINLINE emptyCaseExpr #-}

data PatternBranch = PatternBranch Pos (ProgVar TcStage)

instance HasPos PatternBranch where
  pos (PatternBranch p _) = p

class HasPos a => BranchSpec a where
  displayBranchError :: a -> [ErrorMessage]

instance BranchSpec Void where
  displayBranchError = absurd

instance BranchSpec PatternBranch where
  displayBranchError (PatternBranch _ p) = [Error "branch", Error p]
  {-# INLINE displayBranchError #-}

newtype WildcardBranch = WildcardBranch Pos

instance HasPos WildcardBranch where
  pos (WildcardBranch p) = p
  {-# INLINE pos #-}

instance BranchSpec WildcardBranch where
  displayBranchError _ = [Error "wildcard branch"]
  {-# INLINE displayBranchError #-}

data CondBranch = CondThen Pos | CondElse Pos

instance HasPos CondBranch where
  pos = \case
    CondThen p -> p
    CondElse p -> p
  {-# INLINE pos #-}

instance BranchSpec CondBranch where
  displayBranchError = \case
    CondThen _ -> [Error "‘then’ branch"]
    CondElse _ -> [Error "‘else’ branch"]
  {-# INLINE displayBranchError #-}

branchedConsumeDifference ::
  (BranchSpec a, BranchSpec b) => ProgVar TcStage -> Var -> a -> Pos -> b -> Diagnostic
branchedConsumeDifference name var consumeBranch consumeLoc otherBranch =
  PosError consumeLoc $
    concat
      [ [ Error "Linear variable",
          ErrLine,
          Error "  ",
          Error name,
          Error ":",
          Error (varType var),
          ErrLine,
          Error "is not consumed in"
        ],
        displayBranchError otherBranch,
        [ Error "at",
          Error (pos otherBranch),
          ErrLine,
          Error "but is consumed in"
        ],
        displayBranchError consumeBranch,
        [ Error "at",
          Error (pos consumeBranch)
        ]
      ]
{-# NOINLINE branchedConsumeDifference #-}

branchPatternBindingCount :: Pos -> ProgVar TcStage -> Int -> Int -> Diagnostic
branchPatternBindingCount loc name !expected !given =
  PosError
    loc
    [ Error "The constructor",
      Error name,
      Error "should have",
      Error (pluralZ expected "no arguments," "one argument," (show expected ++ " arguments,")),
      Error "but has been given",
      Error (pluralZ given "none" "one" (show given))
    ]
{-# NOINLINE branchPatternBindingCount #-}

unnecessaryWildcard :: Pos -> Diagnostic
unnecessaryWildcard loc = PosError loc [Error msg]
  where
    msg = "Unnecessary wildcard. All possible branches are handled."
{-# NOINLINE unnecessaryWildcard #-}

wildcardNotAllowed :: Pos -> Pos -> Diagnostic
wildcardNotAllowed wildLoc caseLoc =
  PosError
    wildLoc
    [ Error "Wildcards are not allowed in the context started at",
      Error caseLoc
    ]
{-# NOINLINE wildcardNotAllowed #-}

linearWildcard :: Pos -> TcType -> Diagnostic
linearWildcard loc ty =
  PosError
    loc
    [ Error "Wildcard ignores a linear value of type",
      Error ty
    ]
{-# NOINLINE linearWildcard #-}

protocolConAsValue :: Pos -> ProgVar TcStage -> TypeVar TcStage -> Diagnostic
protocolConAsValue loc con parent =
  PosError
    loc
    [ Error "Constructor",
      Error con,
      Error "is a protocol constructor for type",
      Error parent,
      ErrLine,
      Error "Protocol constructors can only be used in case patterns and arguments to ‘select’."
    ]
{-# NOINLINE protocolConAsValue #-}

builtinMissingApp :: RnExp -> String -> Diagnostic
builtinMissingApp e expected =
  PosError
    (pos e)
    [ Error "Builtin",
      Error e,
      Error "must be followed by",
      Error expected
    ]
{-# NOINLINE builtinMissingApp #-}

unboundVar :: forall stage scope. SingI scope => Pos -> Name stage scope -> Diagnostic
unboundVar loc v =
  PosError
    loc
    [ Error "Unbound",
      Error $ id @String $ eitherName @scope "type variable" "variable",
      Error v
    ]
{-# SPECIALIZE unboundVar :: Pos -> ProgVar TcStage -> Diagnostic #-}
{-# SPECIALIZE unboundVar :: Pos -> TypeVar TcStage -> Diagnostic #-}

undeclaredCon :: forall stage scope. SingI scope => Pos -> Name stage scope -> Diagnostic
undeclaredCon loc v =
  PosError
    loc
    [ Error "Undeclared",
      Error $ id @String $ eitherName @scope "type" "constructor",
      Error v
    ]
{-# SPECIALIZE undeclaredCon :: Pos -> ProgVar TcStage -> Diagnostic #-}
{-# SPECIALIZE undeclaredCon :: Pos -> TypeVar TcStage -> Diagnostic #-}

synthUntypedLambda :: Pos -> Pos -> ProgVar TcStage -> Diagnostic
synthUntypedLambda lamLoc varLoc var =
  PosError
    varLoc
    [ Error "Cannot deduce type of parameter",
      Error var,
      Error "in lambda abstraction at",
      Error lamLoc,
      ErrLine,
      Error "Please provide a type annotation."
    ]
{-# NOINLINE synthUntypedLambda #-}

benchKindMismatch :: Pos -> K.Kind -> Pos -> K.Kind -> Diagnostic
benchKindMismatch p1 k1 p2 k2 =
  PosError p1 . errUnline $
    [ [Error "Kind mismatch in benchmark pragma."],
      [ indent,
        Error "type at",
        Error p1,
        Error "has kind",
        Error k1
      ],
      [ indent,
        Error "type at",
        Error p2,
        Error "has kind",
        Error k2
      ]
    ]
{-# NOINLINE benchKindMismatch #-}

benchTypesEqual :: Pos -> Diagnostic
benchTypesEqual p = PosError p [Error "Types in benchmark are equal."]

showType :: TcType -> Maybe TcType -> [ErrorMessage]
showType t mNF
  | Just tNF <- mNF,
    Alpha t /= Alpha tNF =
      [ Error $ MsgTag " [NF]",
        Error tNF,
        ErrLine,
        Error $ MsgTag "[LIT]",
        Error t
      ]
  | otherwise =
      [Error "     ", Error t]

errUnline :: [[ErrorMessage]] -> [ErrorMessage]
errUnline = List.intercalate [ErrLine]

indent :: ErrorMessage
indent = Error "   "
