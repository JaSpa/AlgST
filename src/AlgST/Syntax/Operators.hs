{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module AlgST.Syntax.Operators where

import AlgST.Syntax.Expression qualified as E
import AlgST.Syntax.Name
import AlgST.Syntax.Pos
import AlgST.Syntax.Type qualified as T
import Control.Applicative
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Language.Haskell.TH.Syntax (Lift)

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

-- | A sequence of un-associated operators interspersed with operands.
data OperatorSequence x
  = OperandFirst !(Maybe (E.Exp x)) (NonEmpty (E.Exp x))
  | OperatorFirst !(Maybe (E.Exp x)) (NonEmpty (E.Exp x))

deriving instance (E.ForallX Lift x, T.ForallX Lift x) => Lift (OperatorSequence x)

instance (E.ForallX HasPos x) => HasPos (OperatorSequence x) where
  pos = pos . NE.head . opSeqExpressions

infixr 9 `opSeqCons`

-- | Prepend a component to an 'OperatorSequence'. This will flip
-- 'OperandFirst' to 'OperatorFirst' and vice versa but it is your job to
-- verify that the prependend compoenent is actually an operand or operator.
opSeqCons :: E.Exp x -> OperatorSequence x -> OperatorSequence x
opSeqCons e = \case
  OperatorFirst rs es -> OperandFirst rs (e <| es)
  OperandFirst rs es -> OperatorFirst rs (e <| es)

opSeqExpressions :: OperatorSequence x -> NonEmpty (E.Exp x)
opSeqExpressions = \case
  OperandFirst _ ne -> ne
  OperatorFirst _ ne -> ne

leftSection :: OperatorSequence x -> Maybe (E.Exp x)
leftSection = \case
  OperandFirst {} -> Nothing
  OperatorFirst _ (e :| _) -> Just e

rightSection :: OperatorSequence x -> Maybe (E.Exp x)
rightSection = \case
  OperandFirst r _ -> r
  OperatorFirst r _ -> r

sectionOperator :: OperatorSequence x -> Maybe (E.Exp x)
sectionOperator ops = leftSection ops <|> rightSection ops

isSection :: OperatorSequence x -> Bool
isSection = isJust . sectionOperator

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
