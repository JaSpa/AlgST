{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module AlgST.Syntax.Operators where

import AlgST.Syntax.Expression qualified as E
import AlgST.Syntax.Name
import AlgST.Util.SourceLocation
import Data.Bifunctor
import Data.Bitraversable
import Data.Foldable
import Data.Function
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Semigroup
import Data.Singletons.TH
import Data.These
import Data.These.Combinators
import Data.Traversable
import Language.Haskell.TH.Syntax (Lift)
import Lens.Family2 hiding ((&))
import Lens.Family2.Unchecked

data Precedence
  = -- | > (<|)
    PBackward
  | -- | > (|>)
    PForward
  | -- | > (||)
    POr
  | -- | > (&&)
    PAnd
  | -- | > (==), (<=), …
    PCmp
  | -- | > (+), (-)
    PAddSub
  | -- | > (*), (/), (%)
    PMulDiv
  deriving (Eq, Ord, Enum, Bounded, Show, Lift)

data Associativity = L | R | NA
  deriving (Show, Eq, Lift)

type OperatorTable = HashMap (NameR Values) (Precedence, Associativity)

-- | Wraps a symbolic operator in parentheses.
operatorValueName :: String -> String
operatorValueName op = "(" ++ op ++ ")"

type Operand = E.Exp

data SequnceHead = HeadO | HeadE

$(genSingletons [''SequnceHead])

data OperatorSequence h x o where
  OpSeqE :: SrcRange -> E.Exp x -> These (NonEmpty (o, E.Exp x)) o -> OperatorSequence HeadE x o
  OpSeqO :: SrcRange -> NonEmpty (o, E.Exp x) -> Maybe o -> OperatorSequence HeadO x o

instance Functor (OperatorSequence h x) where
  fmap = fmapDefault

-- | This instance only folds over the operators. Use 'foldOperatorSequence' to
-- fold over operators and operands.
instance Foldable (OperatorSequence h x) where
  foldMap f = fold . foldOperatorSequence (Just . f) (const Nothing)

instance Traversable (OperatorSequence h x) where
  traverse f (OpSeqE r e tail) =
    OpSeqE r e <$> bitraverse (traverseOperatorPairs f) f tail
  traverse f (OpSeqO r pairs mo) =
    OpSeqO r <$> traverseOperatorPairs f pairs <*> traverse f mo

instance HasRange (OperatorSequence h x o) where
  getRange (OpSeqE r _ _) = r
  getRange (OpSeqO r _ _) = r

instance StoresRange (OperatorSequence h x o) where
  rangeL = lens getRange \case
    OpSeqE _ a b -> \r -> OpSeqE r a b
    OpSeqO _ a b -> \r -> OpSeqO r a b

opsSimpleSeq :: E.Exp x -> o -> E.Exp x -> OperatorSequence HeadE x o
opsSimpleSeq lhs op rhs =
  OpSeqE
    (needRange lhs `runion` needRange op `runion` needRange rhs)
    lhs
    (This (NE.singleton (op, rhs)))

opsLeftSection :: o -> E.Exp x -> OperatorSequence HeadO x o
opsLeftSection o rhs =
  OpSeqO (needRange o `runion` needRange rhs) (NE.singleton (o, rhs)) Nothing

opsRightSection :: E.Exp x -> o -> OperatorSequence HeadE x o
opsRightSection lhs o =
  OpSeqE (needRange lhs `runion` needRange o) lhs (That o)

infixr 9 `consOperator`, `consOperand`

consOperator :: o -> OperatorSequence HeadE x o -> OperatorSequence HeadO x o
consOperator o (OpSeqE r e tail) = OpSeqO (needRange r `runion` needRange o) pairs (justThere tail)
  where
    pairs = case justHere tail of
      Just ps -> (o, e) <| ps
      Nothing -> NE.singleton (o, e)

consOperand :: E.Exp x -> OperatorSequence HeadO x o -> OperatorSequence HeadE x o
consOperand e (OpSeqO r pairs mo) =
  OpSeqE (needRange r `runion` needRange e) e (maybe (This pairs) (These pairs) mo)

foldOperatorSequence :: (Semigroup s) => (o -> s) -> (E.Exp x -> s) -> OperatorSequence h x o -> s
foldOperatorSequence fO fE (OpSeqE _ e tail) =
  fE e <> mergeTheseWith (foldOperatorPairs fO fE) fO (<>) tail
foldOperatorSequence fO fE (OpSeqO _ pairs mo) =
  foldOperatorPairs fO fE pairs & case mo of
    Nothing -> id
    Just o -> (<> fO o)

foldOperatorPairs :: (Semigroup s) => (o -> s) -> (E.Exp x -> s) -> NonEmpty (o, E.Exp x) -> s
foldOperatorPairs fO fE = sconcat . fmap (uncurry (<>) . bimap fO fE)

traverseOperatorPairs :: (Applicative f) => (o -> f o') -> NonEmpty (o, a) -> f (NonEmpty (o', a))
traverseOperatorPairs f = traverse (bitraverse f pure)

-- | Returns 'True' if the given operator sequence is a section.
isSection :: OperatorSequence h x o -> Bool
isSection = isJust . sectionOperator

-- | Retrieve the sequences section operator if there is one.
sectionOperator :: OperatorSequence h x o -> Maybe o
sectionOperator (OpSeqE _ _ tail) = justThere tail
sectionOperator (OpSeqO _ pairs _) = Just $ fst $ NE.head pairs

data SomeOperatorSequence x o where
  SomeOperatorSequence :: (SingI h) => OperatorSequence h x o -> SomeOperatorSequence x o

deriving stock instance Functor (SomeOperatorSequence x)

deriving stock instance Foldable (SomeOperatorSequence x)

deriving stock instance Traversable (SomeOperatorSequence x)

instance HasRange (SomeOperatorSequence x o) where
  getRange = applyOperatorSequence getRange

instance StoresRange (SomeOperatorSequence x o) where
  rangeL = lens getRange $ applyOperatorSequence \ops r ->
    SomeOperatorSequence (ops & rangeL .~ r)

applyOperatorSequence :: (forall h. (SingI h) => OperatorSequence h x o -> a) -> SomeOperatorSequence x o -> a
applyOperatorSequence f (SomeOperatorSequence ops) = f ops

-- | Given a parenthesized operator name returns the unparenthesized version.
pprRawName :: ProgVar stage -> String
pprRawName pv = do
  let name = pprName pv
  let stripClosingParen mkNm ")" = mkNm ""
      stripClosingParen mkNm (c : cs) = stripClosingParen (mkNm . showChar c) cs
      stripClosingParen _ [] = name -- Not correctly parenthesized, fall back to input
  case name of
    '(' : xs -> stripClosingParen id xs
    _ -> name
