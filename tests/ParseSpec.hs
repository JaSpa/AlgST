{-# LANGUAGE CPP #-}
{-# LANGUAGE ImplicitParams #-}

module ParseSpec (spec) where

import AlgST.Parse.Parser
import AlgST.Syntax.Tree
import Control.Monad
import System.FilePath
import Test
import Test.Golden

spec :: Spec
spec = do
  describe "valid" do
    describe "expressions" do
      goldenTests
        (dir "valid/expr")
        (parseTree parseExpr)

    describe "types" do
      goldenTests
        (dir "valid/type")
        (parseTree parseType)

    describe "declarations" do
      goldenTests
        (dir "valid/decl")
        (parseTree parseModule)

  describe "invalid" do
    describe "expressions" do
      goldenTests
        (dir "invalid/expr")
        (expectDiagnostics_ . parseTree parseExpr)

    describe "declarations" do
      goldenTests
        (dir "invalid/decl")
        (expectDiagnostics_ . parseTree parseModule)

  describe "associativity" do
    specify "application" do
      let ?parser = parseType
      "A b c" `shouldParseLike` "((A b) c)"

parseTree :: (LabeledTree a) => Parser a -> String -> Assertion String
parseTree p src = drawLabeledTree <$> shouldParse p src

shouldParseLike :: (?parser :: Parser a, LabeledTree a) => String -> String -> Assertion ()
shouldParseLike src1 src2 =
  join $
    shouldBe
      <$> parseTree ?parser src1
      <*> parseTree ?parser src2

dir :: FilePath -> FilePath
dir sub = dropExtension __FILE__ </> sub
