{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module AlgST.Syntax.Phases
  ( module AlgST.Syntax.Phases,
    module AlgST.Syntax.Pos,
  )
where

import AlgST.Syntax.Name
import AlgST.Syntax.Pos
import Data.Kind

type Phase = Type

type CAll = (Type -> Constraint) -> Phase -> Constraint

type CSame = Phase -> Phase -> Constraint

type CPointwise = (Type -> Type -> Constraint) -> Phase -> Phase -> Constraint

type family XStage x :: Stage

type XName x = Name (XStage x)

type XProgVar x = ProgVar (XStage x)

type XTypeVar x = TypeVar (XStage x)
