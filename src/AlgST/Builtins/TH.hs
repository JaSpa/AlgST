{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-defer-type-errors #-}

module AlgST.Builtins.TH where

import AlgST.Parse.Parser
import AlgST.Rename
import AlgST.Syntax.Module
import AlgST.Syntax.Name
import AlgST.Typing
import AlgST.Util.Error
import Control.Monad
import Control.Monad.Eta
import Control.Monad.Trans
import Control.Monad.Validate
import Data.Foldable
import Language.Haskell.TH hiding (Name)
import Language.Haskell.TH.CodeDo qualified as Code

parseTH ::
  ModuleName -> ModuleMap -> [String] -> CodeQ (ModuleMap, CheckContext, TcModule)
parseTH modName baseMap srcLines = Code.do
  parsed <- case runParserSimple parseModule (unlines srcLines) of
    Left err -> do
      reportError $ "parse error:" ++ err
      pure emptyParsedModule
    Right p -> do
      pure p
  when (not (null (parsedImports parsed))) do
    -- This restriction exists only because there wasn't any necessity to
    -- implement this.
    reportError "Imports not yet supported by ‘parseTH’."
  let (modmap, resolve) = continueRename baseMap modName (parsedModule parsed)
  let check renamed = do
        let doCheck = checkWithModule mempty renamed \runTypeM checked ->
              (checked,) <$> runTypeM extractCheckContext
        mapErrors runErrors $ mapValidateT lift doCheck
  let checked = resolve mempty >>= \(RenameExtra f) -> f check
  case checked of
    Left errs -> Code.do
      traverse_ (reportError . tail . formatErrorMessages Plain "") errs
      [||(modmap, mempty, emptyModule)||]
    Right (tcmod, ctxt) ->
      [||(modmap, ctxt, tcmod)||]
