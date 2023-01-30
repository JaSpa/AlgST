{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module AlgST.Rename.Error
  ( module AlgST.Rename.Error,
    D.MonadErrors,
    D.addError,
    D.fatalError,
  )
where

import AlgST.Syntax.Name
import AlgST.Util.Diagnose qualified as D
import AlgST.Util.SourceLocation
import Data.Coerce
import Data.Function
import Data.Monoid
import Data.Singletons
import Language.Haskell.TH.Syntax (Lift)

data AmbiguousOrigin
  = AmbiguousImport !SrcRange !ModuleName
  | AmbiguousDefine !SrcRange
  deriving stock (Show, Lift)

instance HasRange AmbiguousOrigin where
  getRange = \case
    AmbiguousImport r _ -> r
    AmbiguousDefine r -> r

data NameKind = Con | Var

nameKind :: forall (scope :: Scope) proxy. SingI scope => NameKind -> proxy scope -> String
nameKind k _ =
  eitherName @scope
    (case k of Con -> "type"; Var -> "type variable")
    (case k of Con -> "constructor"; Var -> "variable")

ambiguousUsage :: SrcRange -> NameKind -> NameW scope -> [AmbiguousOrigin] -> D.Diagnostic
ambiguousUsage loc _k _name amb =
  D.err loc "ambigous name" "this usage is ambigous"
    & appEndo (foldMap (coerce choice) amb)
  where
    choice :: AmbiguousOrigin -> D.Diagnostic -> D.Diagnostic
    choice a = D.context (getRange a) (description a)
    description = \case
      AmbiguousImport _ (ModuleName m) -> "it may refer to the import from " ++ m
      AmbiguousDefine {} -> "it may refer to this local definition"
{-# NOINLINE ambiguousUsage #-}

unknownImportItem :: SrcRange -> ModuleName -> Scope -> Located Unqualified -> D.Diagnostic
unknownImportItem stmtLoc modName scope (itemLoc :@ _item) =
  D.err itemLoc "unknown import item" msg
    & D.context stmtLoc "in import statement"
  where
    msg = "this " ++ scopeMsg ++ " is not exported from " ++ unModuleName modName
    scopeMsg = case scope of
      Types -> "type"
      Values -> "name"
{-# NOINLINE unknownImportItem #-}

unboundName ::
  forall stage scope. SingI scope => SrcRange -> NameKind -> Name stage scope -> D.Diagnostic
unboundName loc kind v =
  D.err
    loc
    ("unbound " ++ nameKind kind v)
    "this name is neither defined anywhere in scope nor imported"
{-# NOINLINE unboundName #-}
