{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module AlgST.Parse.Phase where

import AlgST.Parse.Unparser
import AlgST.Syntax.Decl qualified as D
import AlgST.Syntax.Expression qualified as E
import AlgST.Syntax.Module
import AlgST.Syntax.Name
import AlgST.Syntax.Operators
import AlgST.Syntax.Phases
import AlgST.Syntax.Traversal
import AlgST.Syntax.Tree
import AlgST.Syntax.Type qualified as T
import AlgST.Util.SourceLocation
import Data.Void
import Language.Haskell.TH.Syntax (Lift)

data Parse

data ParsedBuiltin
  = BuiltinNew SrcRange
  | BuiltinFork SrcRange
  | BuiltinFork_ SrcRange
  deriving stock (Lift)
  deriving (Show) via ShowUnparse ParsedBuiltin

instance HasRange ParsedBuiltin where
  getRange (BuiltinNew p) = p
  getRange (BuiltinFork p) = p
  getRange (BuiltinFork_ p) = p

instance Unparse ParsedBuiltin where
  unparse =
    unparseConst . \case
      BuiltinNew _ -> "new"
      BuiltinFork _ -> "fork"
      BuiltinFork_ _ -> "fork_"

instance SynTraversable Parse y ParsedBuiltin ParsedBuiltin where
  -- ParsedBuiltin is a leaf type, there is nothing to traverse.
  traverseSyntax _proxy = pure

instance LabeledTree ParsedBuiltin where
  labeledTree _ = [leaf "BuiltinNew"]

{- ORMOLU_DISABLE -}
type PExp = E.Exp Parse
type PBind = E.Bind Parse
type PType = T.Type Parse
type PCaseMap = E.CaseMap Parse
type PModule = Module Parse
type PTypesMap = TypesMap Parse
type PValuesMap = ValuesMap Parse

type PName                    = XName Parse
type PStage                   = Written
type instance XStage    Parse = PStage

type instance E.XLit    Parse = SrcRange
type instance E.XVar    Parse = SrcRange
type instance E.XCon    Parse = SrcRange
type instance E.XAbs    Parse = SrcRange
type instance E.XApp    Parse = SrcRange
type instance E.XPair   Parse = SrcRange
type instance E.XCond   Parse = SrcRange
type instance E.XCase   Parse = SrcRange
type instance E.XTAbs   Parse = SrcRange
type instance E.XTApp   Parse = SrcRange
type instance E.XUnLet  Parse = SrcRange
type instance E.XPatLet Parse = SrcRange
type instance E.XRec    Parse = SrcRange
type instance E.XNew    Parse = Void  -- BuiltinNew
type instance E.XSelect Parse = SrcRange
type instance E.XFork   Parse = Void  -- BuiltinFork
type instance E.XFork_  Parse = Void  -- BuiltinFork_
type instance E.XExp    Parse = Either ParsedBuiltin (SomeOperatorSequence Parse PExp)

type instance E.XBind Parse = SrcRange

type instance T.XUnit    Parse = SrcRange
type instance T.XArrow   Parse = SrcRange
type instance T.XPair    Parse = SrcRange
type instance T.XSession Parse = SrcRange
type instance T.XEnd     Parse = SrcRange
type instance T.XForall  Parse = SrcRange
type instance T.XVar     Parse = SrcRange
type instance T.XCon     Parse = SrcRange
type instance T.XApp     Parse = SrcRange
type instance T.XDualof  Parse = SrcRange
type instance T.XNegate  Parse = SrcRange
type instance T.XType    Parse = Void

type instance D.XAliasDecl    Parse = SrcRange
type instance D.XDataDecl     Parse = SrcRange
type instance D.XProtocolDecl Parse = SrcRange

type instance D.XDataCon      Parse = SrcRange
type instance D.XProtoCon     Parse = SrcRange
{- ORMOLU_ENABLE -}
