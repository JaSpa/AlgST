{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AlgST.Typing.Error where

import AlgST.Parse.Phase qualified as P
import AlgST.Rename
import AlgST.Syntax.Expression qualified as E
import AlgST.Syntax.Kind qualified as K
import AlgST.Syntax.Name
import AlgST.Syntax.Type qualified as T
import AlgST.Typing.Align
import AlgST.Typing.Monad
import AlgST.Typing.Phase
import AlgST.Util
import AlgST.Util.Diagnose qualified as D
import AlgST.Util.Generically
import AlgST.Util.Operators
import AlgST.Util.SourceLocation
import Control.Category ((>>>))
import Control.Monad.Validate
import Data.DList.DNonEmpty qualified as DNE
import Data.Function
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Semigroup
import Data.Singletons
import Data.These
import Data.Void
import Prelude hiding (truncate)

add :: (MonadValidate Errors m) => D.Diagnostic -> m ()
add !e = dispute $ This $ DNE.singleton e

-- | Records multiple errors. If the error list is acutally empty, no errors
-- will be recorded and the computation won't fail.
adds :: (MonadValidate Errors m) => [D.Diagnostic] -> m ()
adds = maybe (pure ()) (dispute . This . DNE.fromNonEmpty) . nonEmpty

fatal :: (MonadValidate Errors m) => D.Diagnostic -> m a
fatal !e = refute $ This $ DNE.singleton e

ifNothing :: (MonadValidate Errors m) => D.Diagnostic -> Maybe a -> m a
ifNothing e = maybe (fatal e) pure

unexpectedKind :: (T.ForallX HasRange x) => T.Type x -> K.Kind -> [K.Kind] -> D.Diagnostic
unexpectedKind t kind hintKinds =
  D.err "kind mismatch"
    & D.primary (getRange t) ("type has kind" <+> D.showSyntax kind)
    & addHint
  where
    addHint = case nonEmpty hintKinds of
      Just hints -> D.hint do
        "Expected a subkind of"
          <+> D.joinOr (D.showSyntax <$> hints)
          <> "."
      Nothing -> id
{-# NOINLINE unexpectedKind #-}

unexpectedForkKind :: P.ParsedBuiltin -> RnExp -> TcType -> K.Kind -> K.Kind -> D.Diagnostic
unexpectedForkKind forkExpr e ty kiActual kiExpected =
  D.err "kind mismatch"
    & D.primary (getRange e) errMsg
    & D.context (getRange forkExpr) (D.showSyntax forkExpr <+> "invoked here")
    & D.note noteMsg
  where
    errMsg =
      D.unlines
        [ "forked expression has type",
          D.indent (D.showSyntax ty),
          "which has kind" <+> D.showSyntax kiActual
        ]
    noteMsg =
      D.unwords
        [ D.showSyntax forkExpr,
          "can only fork expression with a type with kind",
          D.showSyntax kiExpected
        ]
{-# NOINLINE unexpectedForkKind #-}

-- TODO: can we highlight the origin of the check-against type?
typeMismatch :: RnExp -> TcType -> TcType -> TcType -> TcType -> D.Diagnostic
typeMismatch expr tyActual tyActualNF tyExpected tyExpectedNF =
  D.err "type mismatch"
    & D.primary (getRange expr) errMsg
  where
    errMsg =
      D.unlines
        [ "actual type",
          showTypeD tyActual (Just tyActualNF),
          "is not a subtype of",
          showTypeD tyExpected (Just tyExpectedNF)
        ]
{-# NOINLINE typeMismatch #-}

-- TODO: can we highlight the origin of the check-against type?
typeMismatchBind :: K.Bind stage a -> TcType -> RnExp -> D.Diagnostic
typeMismatchBind bind t e =
  D.err "type mismatch"
    & D.primary (getRange bind) errMsg
    & D.context (getRange e) "in this expression"
  where
    errMsg =
      "type variable binder does not align with type"
        <> D.line
        <> D.indent (D.showSyntax t)
{-# NOINLINE typeMismatchBind #-}

-- It is unlikely that this error can be triggered. But I feel that it is
-- better to have an error message at hand should it be needed than crashing
-- the compiler.
noNormalform :: TcType -> D.Diagnostic
noNormalform t =
  D.err "malformed type"
    & D.primary (getRange t) "type has no normal form"
{-# NOINLINE noNormalform #-}

missingUse :: ProgVar TcStage -> Var -> D.Diagnostic
missingUse _name var =
  D.err "linearity violated: missing use"
    & D.primary (getRange var) "linear variable is unused"
    & D.context (varLocation var) ("variable bound here of type" <+> D.showSyntax (varType var))
{-# NOINLINE missingUse #-}

invalidConsumed :: SrcRange -> ProgVar TcStage -> Var -> SrcRange -> D.Diagnostic
invalidConsumed contextLoc _name var loc =
  D.err "linearity violated: use inside unrestricted context"
    & D.primary loc "consumed here"
    & D.context contextLoc "enclosing unrestricted context"
    & D.context (varLocation var) ("variable bound here of type" <+> D.showSyntax (varType var))
{-# NOINLINE invalidConsumed #-}

linVarUsedTwice :: SrcRange -> SrcRange -> ProgVar TcStage -> Var -> D.Diagnostic
linVarUsedTwice loc1 loc2 _name var =
  D.err "linearity violated: multiple uses"
    & D.primary (min loc1 loc2) "first use"
    & D.primary (max loc1 loc2) "second use"
    & D.context (varLocation var) ("variable bound here of type" <+> D.showSyntax (varType var))
{-# NOINLINE linVarUsedTwice #-}

-- TODO: can we highlight more of the context?
noArrowType :: RnExp -> TcType -> D.Diagnostic
noArrowType e t =
  D.err "not a function type"
    & D.primary (getRange e) errMsg
  where
    errMsg =
      "expected a function but this expression's type is not convertible to a function type:"
        <> D.line
        <> showTypeD t Nothing
{-# NOINLINE noArrowType #-}

-- TODO: can we highlight more of the context?
noForallType :: RnExp -> TcType -> D.Diagnostic
noForallType e t =
  D.err "not a forall type"
    & D.primary (getRange e) errMsg
  where
    errMsg =
      "this expression's type is not convertible to a forall type:"
        <> D.line
        <> showTypeD t Nothing
{-# NOINLINE noForallType #-}

typeConstructorNParams :: SrcRange -> NonEmpty RnType -> Int -> Int -> D.Diagnostic
typeConstructorNParams conRange ts !given !expected =
  D.err "invalid type application"
    & D.primary conRange expectMsg
    & D.context (sconcat (getRange <$> ts)) actualMsg
  where
    expectMsg =
      pluralZ
        expected
        "no parameters"
        "one parameter"
        (D.literal expected <+> "parameters")
    actualMsg =
      "but"
        <+> pluralZ
          given
          "none were given"
          "one was given"
          (D.literal given <+> "were given")
{-# NOINLINE typeConstructorNParams #-}

cyclicAliases :: [ExpansionEntry] -> D.Diagnostic
cyclicAliases aliases =
  D.err (plural aliases "cyclic type alias" "cyclic type aliases")
    & appEndo (foldMap (Endo . noteAlias) aliases)
  where
    noteAlias e =
      D.primary (expansionDefRange e) "defined here"
        >>> D.context (expansionUseRange e) "used here"
{-# NOINLINE cyclicAliases #-}

invalidNominalKind :: SrcRange -> String -> TypeVar TcStage -> K.Kind -> NonEmpty K.Kind -> D.Diagnostic
invalidNominalKind loc nomKind name actual allowed =
  D.err ("invalid ‘" <> nomKind <> "’ declaration")
    & D.primary loc errMsg
    & D.note noteMsg
  where
    errMsg =
      D.showSyntax name
        <+> "cannot be declared as kind"
        <+> D.showSyntax actual
    noteMsg =
      D.syntax nomKind
        <+> "declarations can only declare types with"
        <+> plural allowed "kind" "kinds"
        <+> D.joinOr (D.showSyntax <$> allowed)
{-# NOINLINE invalidNominalKind #-}

mismatchedBind :: SrcRange -> ANameG scope -> TcType -> D.Diagnostic
mismatchedBind loc var t =
  D.err "invalid function declaration"
    & D.primary loc ("unexpected binding of" <+> choose "type variable" "variable")
    & D.context (getRange t) "does not align wit declared type"
  where
    choose x y = either (const x) (const y) var
{-# NOINLINE mismatchedBind #-}

invalidPatternExpr :: String -> SrcRange -> TcType -> TcType -> D.Diagnostic
invalidPatternExpr desc loc scrutTy tyNF =
  D.err ("non-matchable subject in " <> desc <> " expression")
    & D.primary loc ("cannot match on values of type" <> D.line <> showTypeD scrutTy (Just tyNF))
{-# NOINLINE invalidPatternExpr #-}

invalidSessionCaseBranch :: E.CaseBranch f Rn -> D.Diagnostic
invalidSessionCaseBranch branch =
  D.err "invalid receiving ‘case’ branch"
    & D.primary (getRange branch) "must bind exactly one variable"
{-# NOINLINE invalidSessionCaseBranch #-}

mismatchedCaseConstructor :: SrcRange -> TcType -> ProgVar TcStage -> D.Diagnostic
mismatchedCaseConstructor conRange ty _con =
  D.err ""
    & D.primary conRange errMsg
    -- TODO: does the `getRange ty` give use the correct range? Probably not.
    & D.context (getRange ty) conMsg
  where
    errMsg =
      "not a type constructor for type"
        <+> D.showSyntax ty
    conMsg =
      "subject of"
        <+> D.syntax "case"
        <+> "expression is of type"
        <+> D.showSyntax ty
{-# NOINLINE mismatchedCaseConstructor #-}

missingCaseBranches :: SrcRange -> NonEmpty (ProgVar TcStage) -> D.Diagnostic
missingCaseBranches loc branches =
  D.err "incomplete pattern match"
    & D.fix loc fixMsg
  where
    fixMsg = case branches of
      c :| [] -> "add a branch for" <+> D.showSyntax c
      _ -> "add branches for" <+> D.joinAnd (D.showSyntax <$> branches)
{-# NOINLINE missingCaseBranches #-}

data PatternBranch = PatternBranch !SrcRange !(ProgVar TcStage)
  deriving stock (Generic)
  deriving (HasRange) via Generically PatternBranch

newtype WildcardBranch = WildcardBranch SrcRange
  deriving stock (Generic)
  deriving (HasRange) via Generically WildcardBranch

data CondBranch = CondThen !SrcRange | CondElse !SrcRange
  deriving stock (Generic)
  deriving (HasRange) via Generically CondBranch

{- ORMOLU_DISABLE -}
class (HasRange a) => BranchSpec a
instance BranchSpec Void
instance BranchSpec PatternBranch
instance BranchSpec WildcardBranch
instance BranchSpec CondBranch
{- ORMOLU_ENABLE -}

branchedConsumeDifference ::
  (BranchSpec a, BranchSpec b) =>
  -- | Variable name.
  ProgVar TcStage ->
  -- | Variable declaration information.
  Var ->
  -- | The branch that consumed the variable.
  a ->
  -- | The range where the variable was consumed.
  SrcRange ->
  -- | The other branch with the unconsumed variable.
  b ->
  D.Diagnostic
branchedConsumeDifference _name var consumeBranch consumeLoc otherBranch =
  D.err "linearity violated: use differes between branches"
    & D.primary consumeLoc "variable consumed here"
    & D.context (getRange consumeBranch) "variable consumed in this branch"
    & D.context (getRange otherBranch) "variable not consumed in this branch"
    & D.context (varLocation var) ("variable bound here of type" <+> D.showSyntax (varType var))
{-# NOINLINE branchedConsumeDifference #-}

branchPatternBindingCount :: SrcRange -> ProgVar TcStage -> Int -> Int -> D.Diagnostic
branchPatternBindingCount loc _name !expected !given =
  D.err ("invalid pattern: " <> mainReason)
    & D.primary loc msg
  where
    mainReason =
      if given < expected
        then "too few constructor arguments"
        else "too many constructor arguments"
    msg =
      "constructor should have"
        <+> pluralZ expected "no arguments," "one argument," (D.literal expected <+> "arguments,")
        <+> "but has been given"
        <+> pluralZ given "none" "one argument" (D.literal given <+> "argument")
{-# NOINLINE branchPatternBindingCount #-}

unnecessaryWildcard :: SrcRange -> D.Diagnostic
unnecessaryWildcard loc =
  D.warn "unnecessary wildcard"
    & D.primary loc "branch will never be taken"
{-# NOINLINE unnecessaryWildcard #-}

wildcardNotAllowed :: SrcRange -> SrcRange -> D.Diagnostic
wildcardNotAllowed wildLoc caseLoc =
  D.err "wildcard pattern not allowed in receiving ‘case’"
    & D.primary wildLoc "wildcard branch appears here"
    & D.context caseLoc ("in this receving" <+> D.syntax "case" <+> "expression")
    & D.hint "Handle all branches covered by the wildcard explicitly."
{-# NOINLINE wildcardNotAllowed #-}

linearWildcard :: SrcRange -> TcType -> D.Diagnostic
linearWildcard loc ty =
  D.err "linearity violated: value unused"
    & D.primary loc ("wildcard ignores a linear value of type" <+> D.showSyntax ty)
{-# NOINLINE linearWildcard #-}

protocolConAsValue :: SrcRange -> ProgVar TcStage -> TypeVar TcStage -> D.Diagnostic
protocolConAsValue loc con parent =
  D.err "protocol constructor used as value"
    & D.primary loc errMsg
    & D.note noteMsg
  where
    errMsg =
      D.showSyntax con
        <+> "is a protocol constructor for"
        <+> D.showSyntax parent
    noteMsg =
      "Protocol constructors can only be used in case patterns and arguments to"
        <+> D.syntax "select"
        <> "."
{-# NOINLINE protocolConAsValue #-}

builtinMissingApp :: RnExp -> D.Doc -> D.Diagnostic
builtinMissingApp e expected =
  D.err "bare builtin"
    & D.primary (getRange e) ("must be followed by" <+> expected)
{-# NOINLINE builtinMissingApp #-}

unboundVar :: forall stage scope. (SingI scope) => SrcRange -> Name stage scope -> D.Diagnostic
unboundVar loc _v =
  D.err ("unbound " <> kind)
    & D.primary loc (D.string kind <+> "used here")
  where
    kind :: String
    kind = eitherName @scope "type variable" "variable"
{-# SPECIALIZE unboundVar :: SrcRange -> ProgVar TcStage -> D.Diagnostic #-}
{-# SPECIALIZE unboundVar :: SrcRange -> TypeVar TcStage -> D.Diagnostic #-}

undeclaredCon :: forall stage scope. (SingI scope) => SrcRange -> Name stage scope -> D.Diagnostic
undeclaredCon loc _v =
  D.err ("unknown " <> kind)
    & D.primary loc (D.string kind <+> "used here")
  where
    kind :: String
    kind = eitherName @scope "type" "constructor"
{-# SPECIALIZE undeclaredCon :: SrcRange -> ProgVar TcStage -> D.Diagnostic #-}
{-# SPECIALIZE undeclaredCon :: SrcRange -> TypeVar TcStage -> D.Diagnostic #-}

synthUntypedLambda :: SrcRange -> SrcRange -> ProgVar TcStage -> D.Diagnostic
synthUntypedLambda lamRange varRange _var =
  D.err "ambigous type"
    & D.primary varRange "cannot deduce type of lambda parameter"
    & D.context lamRange "in lambda abstraction"
    & D.hint "Provide a type signature."
{-# NOINLINE synthUntypedLambda #-}

ambigousExprType :: SrcRange -> D.Diagnostic
ambigousExprType range =
  D.err "ambigous type"
    & D.fix range "provide a type signature"

benchKindMismatch :: SrcRange -> K.Kind -> SrcRange -> K.Kind -> D.Diagnostic
benchKindMismatch p1 k1 p2 k2 =
  D.err "kind mismatch in benchmark pragma"
    & D.primary p1 ("type has kind" <+> D.showSyntax k1)
    & D.primary p2 ("type has kind" <+> D.showSyntax k2)
    & D.note "Heterogeneous type equality is not supported."
{-# NOINLINE benchKindMismatch #-}

benchTypesEqual :: TcType -> TcType -> D.Diagnostic
benchTypesEqual t1 t2 =
  D.err "benchmarked types are equal"
    & D.primary (getRange t1) "first type ..."
    & D.primary (getRange t2) "... is the same as the second type"
{-# NOINLINE benchTypesEqual #-}

showTypeD :: TcType -> Maybe TcType -> D.Doc
showTypeD t mNF = case mNF of
  Just tNF
    | Alpha t /= Alpha tNF ->
        D.tag (D.literal @String " [NF] ")
          <> D.showSyntax tNF
          <> D.line
          <> D.tag (D.literal @String "[LIT] ")
          <> D.showSyntax t
  _ -> D.indent' 6 $ D.showSyntax t
