{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module KindSpec (spec) where

import AlgST.Parse.Parser
import AlgST.Parse.Unparser ()
import AlgST.Syntax.Kind
import AlgST.Util.PartialOrd
import Data.Foldable
import Test

spec :: Spec
spec = do
  it "correctly retrieves multiplicity information" do
    multiplicity P `shouldBe` Nothing :: IO ()
    multiplicity TL `shouldBe` Just Lin
    multiplicity TU `shouldBe` Just Un
    multiplicity SL `shouldBe` Just Lin
    multiplicity SU `shouldBe` Just Un

  it "parses all kinds correctly" do
    for_ allKinds \k -> do
      k' <- shouldParse parseKind (show k)
      k' `shouldBe` k

  describe "partial order" do
    it "is reflexive" do
      allKinds `allShouldSatisfy` \k ->
        k <=? k

    it "is antisymmetric" do
      let ks = (,) <$> allKinds <*> allKinds
      ks `allShouldSatisfy` \(k1, k2) ->
        k1 <=? k2 && k2 <=? k1 ==> k1 == k2

    it "is transitive" do
      let ks = (,,) <$> allKinds <*> allKinds <*> allKinds
      ks `allShouldSatisfy` \(k1, k2, k3) ->
        k1 <=? k2 && k2 <=? k3 ==> k1 <=? k3

    it "P is the superkind of all kinds" do
      allKinds `allShouldSatisfy` \k ->
        k <=? P

      allKinds `allShouldSatisfy` \k ->
        (P <=? k) == (k == P)

  describe "least upper bound" do
    it "returns a known kind" do
      let ks = (,) <$> allKinds <*> allKinds
      ks `allShouldSatisfy` \(k1, k2) ->
        leastUpperBound k1 k2 `elem` allKinds

    it "returns a superkind of both" do
      let ks = (,) <$> allKinds <*> allKinds
      ks `allShouldSatisfy` \(k1, k2) -> do
        let k = leastUpperBound k1 k2
        k1 <=? k && k2 <=? k

    it "has P as the maximum kind" do
      allKinds `allShouldSatisfy` \k ->
        leastUpperBound k P == P

    it "is commutative" do
      let ks = (,) <$> allKinds <*> allKinds
      ks `allShouldSatisfy` \(k1, k2) ->
        leastUpperBound k1 k2 == leastUpperBound k2 k1

    it "is associative" do
      let ks = (,,) <$> allKinds <*> allKinds <*> allKinds
      ks `allShouldSatisfy` \(k1, k2, k3) -> do
        let a = do
              let k = leastUpperBound k2 k3
              leastUpperBound k1 k
        let b = do
              let k = leastUpperBound k1 k2
              leastUpperBound k k3
        a == b

    it "is idempotent" do
      let ks = (,) <$> allKinds <*> allKinds
      ks `allShouldSatisfy` \(k1, k2) -> do
        let k = leastUpperBound k1 k2
        k == leastUpperBound k1 k

      ks `allShouldSatisfy` \(k1, k2) -> do
        let k = leastUpperBound k1 k2
        k == leastUpperBound k2 k

allKinds :: [Kind]
allKinds = [TL, TU, SL, SU, P]

infix 1 ==>

class Implication a b | a -> b where
  (==>) :: a -> b -> Bool

instance Implication Bool Bool where
  False ==> _ = True
  True ==> x = x

instance Implication (Maybe a) (a -> Bool) where
  Nothing ==> _ = True
  Just a ==> p = p a

allShouldSatisfy :: (HasCallStack, Show a, Foldable f) => f a -> (a -> Bool) -> Expectation
allShouldSatisfy as f = for_ as \a -> a `shouldSatisfy` f
