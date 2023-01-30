{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
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
import AlgST.Util.SourceLocation (SrcRange)
import Data.Void
import Language.Haskell.TH.Syntax (Lift)

data Parse

data ParsedBuiltin
  = BuiltinNew Pos
  | BuiltinFork Pos
  | BuiltinFork_ Pos
  deriving stock (Lift)

instance HasPos ParsedBuiltin where
  pos (BuiltinNew p) = p
  pos (BuiltinFork p) = p
  pos (BuiltinFork_ p) = p

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

type instance E.XLit    Parse = Pos
type instance E.XVar    Parse = Pos
type instance E.XCon    Parse = Pos
type instance E.XAbs    Parse = Pos
type instance E.XApp    Parse = Pos
type instance E.XPair   Parse = Pos
type instance E.XCond   Parse = Pos
type instance E.XCase   Parse = Pos
type instance E.XTAbs   Parse = Pos
type instance E.XTApp   Parse = Pos
type instance E.XUnLet  Parse = Pos
type instance E.XPatLet Parse = Pos
type instance E.XRec    Parse = Pos
type instance E.XNew    Parse = Void  -- BuiltinNew
type instance E.XSelect Parse = Pos
type instance E.XFork   Parse = Void  -- BuiltinFork
type instance E.XFork_  Parse = Void  -- BuiltinFork_
type instance E.XExp    Parse = Either ParsedBuiltin (SomeOperatorSequence Parse PExp)

type instance E.XBind Parse = Pos

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
