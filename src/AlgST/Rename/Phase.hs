{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module AlgST.Rename.Phase
  ( module AlgST.Rename.Phase,
    module AlgST.Syntax.Pos,
  )
where

import AlgST.Parse.Phase (ParsedBuiltin)
import AlgST.Syntax.Decl qualified as D
import AlgST.Syntax.Expression qualified as E
import AlgST.Syntax.Module (Module)
import AlgST.Syntax.Name
import AlgST.Syntax.Phases
import AlgST.Syntax.Pos
import AlgST.Syntax.Type qualified as T
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

type instance T.XUnit    Rn = Pos
type instance T.XArrow   Rn = Pos
type instance T.XPair    Rn = Pos
type instance T.XSession Rn = Pos
type instance T.XEnd     Rn = Pos
type instance T.XForall  Rn = Pos
type instance T.XVar     Rn = Pos
type instance T.XCon     Rn = Pos
type instance T.XApp     Rn = Pos
type instance T.XDualof  Rn = Pos
type instance T.XNegate  Rn = Pos
type instance T.XType    Rn = Void

type instance D.XAliasDecl    Rn = Pos
type instance D.XDataDecl     Rn = Pos
type instance D.XProtocolDecl Rn = Pos

type instance D.XDataCon      Rn = Pos
type instance D.XProtoCon     Rn = Pos
{- ORMOLU_ENABLE -}
