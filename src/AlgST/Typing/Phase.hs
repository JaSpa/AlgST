{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module AlgST.Typing.Phase where

import AlgST.Parse.Unparser
import AlgST.Syntax.Decl qualified as D
import AlgST.Syntax.Expression qualified as E
import AlgST.Syntax.Kind qualified as K
import AlgST.Syntax.Module (Module)
import AlgST.Syntax.Name
import AlgST.Syntax.Phases
import AlgST.Syntax.Traversal
import AlgST.Syntax.Tree
import AlgST.Syntax.Type qualified as T
import AlgST.Util.SourceLocation
import Data.Functor.Const
import Data.Functor.Identity
import Data.Void
import Language.Haskell.TH.Syntax (Lift)

-- | Typing phase token.
--
-- After typechecking all expressions are annotated with their type, type
-- aliases are expanded and type constructors are represented as 'TypeRef'.
data Tc

instance SynTraversable Tc Tc TcExpX TcExpX where
  traverseSyntax proxy = \case
    ValueCase p exp map ->
      ValueCase p
        <$> traverseSyntax proxy exp
        <*> traverseSyntax proxy map
    RecvCase p exp map ->
      RecvCase p
        <$> traverseSyntax proxy exp
        <*> traverseSyntax proxy map

instance LabeledTree TcExpX where
  labeledTree =
    pure . \case
      RecvCase _ exp map ->
        tree
          "TcExpX.RecvCase"
          [labeledTree exp, fieldMapTree map]
      ValueCase _ exp map ->
        tree
          "TcExpX.ValueCase"
          [labeledTree exp, fieldMapTree map]

instance HasRange TcExpX where
  getRange = \case
    ValueCase r _ _ -> r
    RecvCase r _ _ -> r

data TcExpX
  = -- | > ValueCase _ e cases        ~ case e of { cases... }
    ValueCase !SrcRange !TcExp !(TcCaseMap [] Maybe)
  | -- | > RecvCase _ e cases         ~ case e of { cases... }
    RecvCase !SrcRange !TcExp !(TcCaseMap Identity (Const ()))
  deriving stock (Lift)

instance Unparse TcExpX where
  unparse = \case
    ValueCase _ e cm -> unparseCase e cm
    RecvCase _ e cm -> unparseCase e cm

-- | Replaces @"AlgST.Syntax.Type".'T.Con'@/@"AlgST.Syntax.Type".'T.App'@
-- combinations after type checking.
data TypeRef = TypeRef
  { typeRefName :: !(TypeVar TcStage),
    -- | Constructor names excluded from this type.
    typeRefExcl :: !(TcNameMap Values SrcRange),
    typeRefArgs :: [TcType],
    typeRefKind :: !K.Kind,
    typeRefNameRange :: !SrcRange
  }
  deriving stock (Lift)

instance HasRange TypeRef where
  getStartLoc = getStartLoc . typeRefNameRange
  getEndLoc tr = maximum $ getEndLoc (typeRefNameRange tr) : fmap getEndLoc (typeRefArgs tr)

instance SynTraversable Tc Tc TypeRef TypeRef where
  traverseSyntax proxy ref = do
    args <- traverse (traverseSyntax proxy) (typeRefArgs ref)
    -- This is not a noop! Even though it looks like a `traverse pure` (which
    -- would be a noop) the implementation of `traverseNameMap` calls into
    -- `useConstructor` which is caller supplied.
    excl <- traverseNameMap proxy pure (typeRefExcl ref)
    pure
      TypeRef
        { -- We are explicit in the fields so that an error is generated if there
          -- are fields added which might require traversing.
          typeRefName = typeRefName ref,
          typeRefArgs = args,
          typeRefExcl = excl,
          typeRefKind = typeRefKind ref,
          typeRefNameRange = typeRefNameRange ref
        }

instance Unparse TypeRef where
  unparse r = unparseApp (pprName (typeRefName r)) (typeRefArgs r)

instance LabeledTree TypeRef where
  -- We can't include the whole typeRefDecl since the type graph might be
  -- recursive and the LabeledTree class is not equipped to handle cyclic
  -- structures.
  labeledTree ref =
    -- TODO: include typeRefExcl
    pure . Node "TypeRef" $
      leaf (pprName (typeRefName ref))
        : concatMap labeledTree (typeRefArgs ref)

-- | Returns the type's kind.
--
-- This function does no checking but assumes that the given type is well
-- formed. It does the minmal amount of work to determine the type's kind.
typeKind :: TcType -> K.Kind
typeKind t = case t of
  T.Unit _ -> K.TU
  T.Arrow _ m _ _ -> K.Kind K.Top m
  T.Pair _ t u ->
    K.leastUpperBound (typeKind t) (typeKind u)
  T.Session {} -> K.SL
  T.End _ _ -> K.SL
  T.Forall _ (K.Bind _ _ _ t) ->
    maybe malformed (K.Kind K.Top) (K.multiplicity (typeKind t))
  T.Var k _ -> unL k
  T.Con x _ _ -> absurd x
  T.App x _ _ -> absurd x
  T.Dualof _ t -> typeKind t
  T.Negate _ _ -> K.P
  T.Type r -> typeRefKind r
  where
    malformed = error $ "internal error: malformed type " ++ show t

tcDeclKind :: D.TypeDecl Tc -> K.Kind
tcDeclKind = \case
  D.AliasDecl x _ -> absurd x
  D.DataDecl _ d -> D.nominalKind d
  D.ProtoDecl _ d -> D.nominalKind d

{- ORMOLU_DISABLE -}
type TcName                = XName Tc
type TcExp                 = E.Exp Tc
type TcType                = T.Type Tc
type TcBind                = E.Bind Tc
type TcCaseMap f g         = E.CaseMap' f g Tc
type TcModule              = Module Tc
type TcProtocolSubset      = T.ProtocolSubset Resolved

type TcStage               = Resolved
type TcNameMap scope       = NameMapG TcStage scope
type TcNameSet scope       = NameSetG TcStage scope
type instance XStage    Tc = TcStage

type instance E.XLit    Tc = SrcRange
type instance E.XVar    Tc = SrcRange
type instance E.XCon    Tc = SrcRange
type instance E.XAbs    Tc = SrcRange
type instance E.XApp    Tc = SrcRange
type instance E.XPair   Tc = SrcRange
type instance E.XCond   Tc = Void     -- Desugared to @'E.Exp' ('ValueCase' _ _)@.
type instance E.XCase   Tc = Void     -- E.Exp ValueCase / E.Exp RecvCase
type instance E.XTAbs   Tc = SrcRange
type instance E.XTApp   Tc = SrcRange
type instance E.XUnLet  Tc = Void     -- Desugared to 'Case'.
type instance E.XPatLet Tc = Void     -- Desugared to 'Case'.
type instance E.XRec    Tc = SrcRange
type instance E.XNew    Tc = SrcRange
type instance E.XSelect Tc = SrcRange
type instance E.XFork   Tc = SrcRange      -- TODO: Could be desugared to 'New' + 'Fork_' + ...
type instance E.XFork_  Tc = SrcRange
type instance E.XExp    Tc = TcExpX
type instance E.XBind   Tc = SrcRange

type instance T.XUnit    Tc = SrcRange
type instance T.XArrow   Tc = SrcRange
type instance T.XPair    Tc = SrcRange
type instance T.XSession Tc = SrcRange
type instance T.XEnd     Tc = SrcRange
type instance T.XForall  Tc = SrcRange
type instance T.XVar     Tc = Located K.Kind
type instance T.XCon     Tc = Void  -- Con/App nodes are replaced by TypeRef nodes.
type instance T.XApp     Tc = Void
type instance T.XDualof  Tc = SrcRange
type instance T.XNegate  Tc = SrcRange
type instance T.XType    Tc = TypeRef

type instance D.XAliasDecl    Tc = Void  -- Type aliases have been expanded after type checking.
type instance D.XDataDecl     Tc = SrcRange
type instance D.XProtocolDecl Tc = SrcRange

type instance D.XDataCon      Tc = SrcRange
type instance D.XProtoCon     Tc = SrcRange
{- ORMOLU_ENABLE -}
