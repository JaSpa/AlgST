{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-defer-type-errors #-}

module AlgST.Builtins.TH (parseTH) where

import AlgST.Parse.Parser
import AlgST.Rename
import AlgST.Syntax.Module
import AlgST.Syntax.Name
import AlgST.Typing
import AlgST.Util.Diagnose qualified as D
import AlgST.Util.SourceManager
import Control.Category ((>>>))
import Control.Monad
import Control.Monad.Eta
import Control.Monad.Trans
import Control.Monad.Validate
import Data.Foldable
import Language.Haskell.TH hiding (Name)
import Language.Haskell.TH.CodeDo qualified as Code
import Prettyprinter.Render.String qualified as P

parseTH ::
  ModuleName -> ModuleMap -> [String] -> CodeQ (ModuleMap, CheckContext, TcModule)
parseTH modName baseMap srcLines = Code.do
  -- Create the source manager.
  let srcBuffer = encodedBuffer "" (unlines srcLines)
  let mgr = singletonManager srcBuffer

  -- Run the parser.
  parsed <- case runParser parseModule (bufferContents srcBuffer) of
    Left errs -> do
      reportDiagnostics mgr errs
      pure emptyParsedModule
    Right p -> do
      pure p

  -- Check for some unsupported constructs.
  when (not (null (parsedImports parsed))) do
    -- This restriction exists only because there wasn't any necessity to
    -- implement this.
    reportError "Imports not yet supported by ‘parseTH’."

  -- Renaming & typechecking.
  let (modmap, resolve) = continueRename baseMap modName (parsedModule parsed)
  let check renamed = do
        let doCheck = checkWithModule mempty renamed \runTypeM checked ->
              (checked,) <$> runTypeM extractCheckContext
        mapErrors (undefined runErrors) $ mapValidateT lift doCheck
  let checked = resolve mempty >>= \(RenameExtra f) -> f check
  case checked of
    Left errs -> Code.do
      reportDiagnostics mgr errs
      [||(modmap, mempty, emptyModule)||]
    Right (tcmod, ctxt) ->
      [||(modmap, ctxt, tcmod)||]

reportDiagnostics :: (Foldable t) => SourceManager -> t D.Diagnostic -> Q ()
reportDiagnostics mgr =
  toList
    >>> D.buildSorted mgr
    >>> D.layoutDiagnostic D.WithUnicode (D.AvailablePerLine 80 1.0)
    >>> P.renderString
    >>> reportError
