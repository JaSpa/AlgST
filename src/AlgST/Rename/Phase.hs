{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module AlgST.Rename.Phase where

import AlgST.Parse.Phase (ParsedBuiltin)
import AlgST.Syntax.Decl qualified as D
import AlgST.Syntax.Expression qualified as E
import AlgST.Syntax.Module (Module)
import AlgST.Syntax.Name
import AlgST.Syntax.Phases
import AlgST.Syntax.Type qualified as T
import AlgST.Util.SourceLocation (SrcRange)
import Data.Void

-- | Type level tag for renamed syntax elements.
data Rn

{- ORMOLU_DISABLE -}
type RnName     = XName     Rn
type RnExp      = E.Exp     Rn
type RnBind     = E.Bind    Rn
type RnCaseMap  = E.CaseMap Rn
type RnModule   = Module    Rn
type RnType     = T.Type    Rn

type RnStage               = Resolved
type instance XStage    Rn = RnStage

type instance E.XLit    Rn = Pos
type instance E.XVar    Rn = Pos
type instance E.XCon    Rn = Pos
type instance E.XAbs    Rn = Pos
type instance E.XApp    Rn = Pos
type instance E.XPair   Rn = Pos
type instance E.XCond   Rn = Pos
type instance E.XCase   Rn = Pos
type instance E.XTAbs   Rn = Pos
type instance E.XTApp   Rn = Pos
type instance E.XUnLet  Rn = Pos
type instance E.XPatLet Rn = Pos
type instance E.XRec    Rn = Pos
type instance E.XNew    Rn = Void  -- BuiltinNew
type instance E.XSelect Rn = Pos
type instance E.XFork   Rn = Void  -- BuiltinFork
type instance E.XFork_  Rn = Void  -- BuiltinFork_
type instance E.XExp    Rn = ParsedBuiltin

type instance E.XBind Rn = Pos

type instance T.XUnit    Rn = SrcRange
type instance T.XArrow   Rn = SrcRange
type instance T.XPair    Rn = SrcRange
type instance T.XSession Rn = SrcRange
type instance T.XEnd     Rn = SrcRange
type instance T.XForall  Rn = SrcRange
type instance T.XVar     Rn = SrcRange
type instance T.XCon     Rn = SrcRange
type instance T.XApp     Rn = SrcRange
type instance T.XDualof  Rn = SrcRange
type instance T.XNegate  Rn = SrcRange
type instance T.XType    Rn = Void

type instance D.XAliasDecl    Rn = SrcRange
type instance D.XDataDecl     Rn = SrcRange
type instance D.XProtocolDecl Rn = SrcRange

type instance D.XDataCon      Rn = SrcRange
type instance D.XProtoCon     Rn = SrcRange
{- ORMOLU_ENABLE -}
