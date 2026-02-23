{-# LANGUAGE TypeFamilies #-}

module AlgST.Util where

import AlgST.Syntax.Pos
import Data.Char qualified as C
import Data.Foldable
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.Ord
import Data.Sequence (Seq)
import Prelude hiding (truncate)

joinConnector :: String -> NonEmpty String -> ShowS
joinConnector _ (x :| []) =
  showString x
joinConnector c (x :| [y]) =
  showString x . showString ", " . showString c . showChar ' ' . showString y
joinConnector c (x :| y : ys) =
  showString x . showString ", " . joinConnector c (y :| ys)

joinOr :: NonEmpty String -> String
joinOr xs = joinConnector "or" xs ""

joinAnd :: NonEmpty String -> String
joinAnd xs = joinConnector "and" xs ""

whenJust :: (Applicative f) => Maybe a -> (a -> f ()) -> f ()
whenJust = for_

-- Like @take n xs@ but adds an extra item in case some elements were
-- discarded.
truncate :: Int -> a -> [a] -> [a]
truncate n a as = zipWith fromMaybe as $ replicate n Nothing ++ [Just a]

truncate' :: Int -> [a] -> [a] -> [a]
truncate' n trunc as = concat $ truncate n trunc (pure <$> as)

class Counted a where
  pluralZ :: (zero ~ b, one ~ b, many ~ b) => a -> zero -> one -> many -> b

{- ORMOLU_DISABLE -}
instance Counted Int where
  pluralZ 0  zero _one _many = zero
  pluralZ 1 _zero  one _many = one
  pluralZ _ _zero _one  many = many

instance Counted [a] where
  pluralZ []   zero _one _many = zero
  pluralZ [_] _zero  one _many = one
  pluralZ _   _zero _one  many = many
{- ORMOLU_ENABLE -}

instance Counted (NonEmpty a) where
  pluralZ (_ :| as) _zero one many = pluralZ as one many many

instance Counted (Seq a) where
  pluralZ = pluralZ . toList

plural :: (Counted a, one ~ b, many ~ b) => a -> one -> many -> b
plural a one many = pluralZ a many one many

sortPos :: (HasPos a) => [a] -> [a]
sortPos =
  -- The @HasPos@ is usually very easy to access, there is no need for the
  -- decorate-sort-undecorate paradigm used by 'sortOn'.
  List.sortBy (comparing pos)

sortPos' :: [(Pos, a)] -> [(Pos, a)]
sortPos' =
  -- The @HasPos@ is usually very easy to access, there is no need for the
  -- decorate-sort-undecorate paradigm used by 'sortOn'.
  List.sortBy (comparing fst)

mguard :: (Monoid m) => Bool -> m -> m
mguard True m = m
mguard False _ = mempty

capitalise :: String -> String
capitalise [] = []
capitalise (x : xs) = C.toUpper x : xs
