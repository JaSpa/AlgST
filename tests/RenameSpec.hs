{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RenameSpec (spec) where

import AlgST.Builtins
import AlgST.Parse.Parser
import AlgST.Rename
import AlgST.Rename.Error (AmbiguousOrigin (..))
import AlgST.Rename.Fresh
import AlgST.Rename.Modules
import AlgST.Syntax.Name
import AlgST.Syntax.Tree
import AlgST.Util.SourceLocation
import Control.Monad.Reader
import Control.Monad.Validate
import Data.Function
import Data.Map.Strict qualified as Map
import Data.Traversable
import System.FilePath
import Test
import Test.Golden

spec :: Spec
spec = do
  describe "valid" do
    describe "expressions" do
      goldenTests (dir "valid/expr") parseRenameExpr

    describe "declarations" do
      goldenTests (dir "valid/prog") do
        -- TODO: Verify the resulting module map as well.
        shouldParse parseDecls
          >=> shouldNotError
          . renameModule (ModuleName "M") renameEnv
          >>> fmap drawLabeledTree

  describe "invalid" do
    describe "expressions" do
      goldenTests
        (dir "invalid/expr")
        (expectDiagnostics_ . parseRenameExpr)

parseRenameExpr :: String -> Assertion String
parseRenameExpr src = do
  expr <- shouldParse parseExpr src
  renameSyntax expr
    & runValidateT
    & flip runReaderT (emptyModuleMap, renameEnv)
    & runFresh (ModuleName "M")
    & fmap drawLabeledTree
    & shouldNotError

renameEnv :: RenameEnv
renameEnv =
  builtinsEnv
    <> RenameEnv
      { rnTyVars = mconcat types,
        rnProgVars = mconcat values
      }
  where
    (rid, values) =
      mapAccumL binding firstResolvedId . fmap Unqualified $
        [ "global",
          "C1",
          "C2"
        ]
    (_, types) =
      mapAccumL binding rid . fmap Unqualified $
        []
    binding :: ResolvedId -> Unqualified -> (ResolvedId, Bindings scope)
    binding !rid u = do
      let n = Name emptyModuleName u
      let resolved =
            PartialResolve $
              Map.singleton
                (ResolvedName n (ModuleName "R") rid)
                (AmbiguousDefine NullRange)
      (nextResolvedId rid, Bindings (Map.singleton n resolved))

dir :: FilePath -> FilePath
dir sub = dropExtension __FILE__ </> sub
