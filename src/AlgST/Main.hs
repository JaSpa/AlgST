{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AlgST.Main (main) where

import AlgST.Benchmark qualified as Bench
import AlgST.Builtins
import AlgST.CommandLine
import AlgST.Driver (Settings (..))
import AlgST.Driver qualified as Driver
import AlgST.Driver.Output
import AlgST.Interpret qualified as I
import AlgST.Parse.Parser qualified as P
import AlgST.Parse.Phase (Parse)
import AlgST.Rename
import AlgST.Syntax.Module
import AlgST.Syntax.Name
import AlgST.Syntax.Traversal
import AlgST.Typing (Tc)
import AlgST.Typing qualified as Tc
import AlgST.Typing.Align
import AlgST.Util qualified as Util
import AlgST.Util.Diagnose qualified as D
import AlgST.Util.Error
import AlgST.Util.Output
import AlgST.Util.SourceManager
import Control.Category ((>>>))
import Control.Exception
import Control.Monad
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.DList.DNonEmpty qualified as DNE
import Data.Either
import Data.Foldable
import Data.Function
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty (NonEmpty)
import Data.Traversable
import Main.Utf8
import System.Console.ANSI qualified as ANSI
import System.Exit
import System.FilePath qualified as FP
import System.IO

main :: IO ()
main = withUtf8 do
  runOpts <- getOptions
  stderrMode <- maybe (discoverMode stderr) pure (optsOutputMode runOpts)

  let allowAnsi = stderrMode /= Plain
  withOutput allowAnsi stderr \outputHandle -> do
    checkResult <- case optsSource runOpts of
      -- No custom source, only builtins.
      Nothing -> do
        let rn = Driver.RenameExtra builtinsEnv builtinsModuleMap
        let tc = Driver.CheckExtra rn builtinsModuleCtxt
        let result = Driver.Result builtinsModule tc
        pure $ HM.singleton MainModule result
      -- Run the source code and all imported modules through parsing, renaming
      -- and type checking.
      Just src ->
        checkSources runOpts outputHandle stderrMode src
          >>= maybe exitFailure pure

    -- Use the checked results to answer any queries.
    queriesGood <-
      answerQueries
        outputHandle
        stderrMode
        (optsQueries runOpts)
        checkResult

    -- If benchmarks were requested run them now.
    benchGood <-
      case optsBenchmarksOutput runOpts of
        Nothing -> pure True
        Just fp -> Bench.run outputHandle stderrMode fp checkResult

    -- Run the interpreter if requested.
    runGood <-
      if optsDoEval runOpts
        then runInterpret runOpts outputHandle stderrMode checkResult
        else pure True

    when (not queriesGood || not benchGood || not runGood) do
      exitFailure

checkSources ::
  RunOpts ->
  OutputHandle ->
  OutputMode ->
  Source ->
  IO (Maybe (HashMap ModuleName (Driver.Result Tc)))
checkSources runOpts outH outMode mainSource = do
  (mainSource, srcManager) <- case mainSource of
    SourceFile fp -> do
      (buffer, srcManager) <- insertFile fp emptyManager
      pure (Just buffer, srcManager)
    SourceStdin -> do
      -- If the input comes from the terminal and either of the output
      -- streams goes to the terminal we output a separating newline.
      stdinTerm <- hIsTerminalDevice stdin
      stdoutTerm <- hIsTerminalDevice stdout
      stderrTerm <- hIsTerminalDevice stderr
      let termOut
            | stdinTerm && stdoutTerm = Just stdout
            | stdinTerm && stderrTerm = Just stderr
            | otherwise = Nothing
      (buffer, srcManager) <- insertStdin "«stdin»" emptyManager
      for_ termOut \h -> hPutChar h '\n'
      pure (Just buffer, srcManager)
    SourceMain ->
      -- We expect the driver to find the Main module through its usual
      -- module lookup mechanism.
      pure (Nothing, emptyManager)

  let driverSettings =
        maybe id (Driver.addModuleSource MainModule) mainSource $
          Driver.defaultSettings
            { driverSequential = optsDriverSeq runOpts,
              driverQuietProgress = optsQuiet runOpts,
              driverVerboseDeps = optsDriverDeps runOpts,
              driverVerboseSearches = optsDriverModSearch runOpts,
              driverSearchPaths = FP.normalise <$> optsDriverPaths runOpts,
              driverOutputMode = outMode,
              driverOutputHandle = outH
            }

  mcheckResult <- Driver.runComplete driverSettings
  when (not (optsQuiet runOpts)) do
    outputLnS outH case mcheckResult of
      Just _ -> applyStyle outMode (styleBold . styleFG ANSI.Green) (showString "Success.")
      Nothing -> applyStyle outMode (styleBold . styleFG ANSI.Red) (showString "Failed.")
  pure $ Driver.compactResults <$> mcheckResult

answerQueries ::
  OutputHandle ->
  OutputMode ->
  [Query String] ->
  HashMap ModuleName (Driver.Result Tc) ->
  IO Bool
answerQueries out outMode queries checkResult = do
  and <$> for queries \case
    QueryTySynth s ->
      parseRename P.parseExpr s tysynth
        & fmap showTySynth
        & printResult "--type" s
    QueryKiSynth s ->
      parseRename P.parseType s (fmap snd . Tc.kisynth)
        & fmap (pure . show)
        & printResult "--kind" s
    QueryNF s ->
      parseRename P.parseType s (Tc.kisynth >=> Tc.normalize . fst)
        & fmap (pure . show)
        & printResult "--nf" s
  where
    queryEnv =
      foldMap
        (Driver.rxRenameEnv . Driver.cxRename . Driver.resultExtra)
        (HM.lookup MainModule checkResult)
    queryCtxt =
      foldMap
        (Driver.cxContext . Driver.resultExtra)
        (HM.lookup MainModule checkResult)

    tysynth expr = do
      t <- fmap snd $ Tc.tysynth expr
      tNF <- Tc.normalize t
      let !res
            | Alpha t == Alpha tNF = Left t
            | otherwise = Right (t, tNF)
      pure res

    showTySynth = either (pure . show) \(t, tNF) ->
      [ applyStyle outMode styleBold (showString "[SYN] ") (show t),
        applyStyle outMode styleBold (showString " [NF] ") (show tNF)
      ]

    parseRename ::
      (SynTraversable Parse Rn (s Parse) (s Rn)) =>
      P.Parser (s Parse) ->
      Buffer ->
      (s Rn -> Tc.TypeM a) ->
      Either D.Errors a
    parseRename p buf f = do
      parsed <- P.runParser p (bufferContents buf)
      RenameExtra extra <-
        renameModuleExtra (ModuleName "Q") emptyModule
          & snd
          & ($ queryEnv)
          & first DNE.toNonEmpty
      first DNE.toNonEmpty $ extra \_ -> do
        renamed <- renameSyntax parsed
        Tc.checkResultAsRnM $ Tc.checkWithModule queryCtxt emptyModule \runTypeM _ ->
          runTypeM $ f renamed

    printResult :: String -> String -> Either (NonEmpty Diagnostic) [String] -> IO Bool
    printResult heading src = \case
      Left errs -> do
        outputLnS out $ prefix . showChar '\n' . renderErrors' (Just 5) outMode "" (toList errs)
        pure False
      Right lns -> do
        outputLnS out prefix
        sequence_ [outputLnS out $ showString $ "  " ++ s | s <- lns]
        pure True
      where
        prefix =
          showChar '\n'
            . applyStyle outMode (styleBold . styleFG ANSI.Cyan) (showString heading)
            . showChar ' '
            . applyStyle outMode (styleFG ANSI.Cyan) (showString (truncateSource src))

    truncateSource :: String -> String
    truncateSource =
      lines >>> \case
        [] -> ""
        [ln] -> Util.truncate' 60 "..." ln
        ln : _ -> take 60 ln ++ "..."

runInterpret ::
  RunOpts -> OutputHandle -> OutputMode -> HashMap ModuleName (Driver.Result Tc) -> IO Bool
runInterpret opts out outMode checkedModules = do
  let mmainName = do
        HM.lookup MainModule checkedModules
          >>= Driver.lookupRenamed MainFunction
  outputStrLn out ""
  case mmainName of
    Nothing -> do
      outputError out outMode "No ‘main’ to run."
      pure False
    Just mainName -> do
      clearSticky out
      outputStrLn out "Running ‘main’"
      hSetBuffering stderr LineBuffering
      result <- try do
        let env = foldMap (I.programEnvironment . Driver.resultModule) checkedModules
        I.runEvalWith
          (I.defaultSettings {I.evalBufferSize = optsBufferSize opts})
          env
          (I.evalName mainName)
      clearSticky out
      case result of
        Left ex
          -- Don't catch async exceptions as these are usually not meant to be
          -- recoverable/we want to exit as fast as possible. For example,
          -- CTRL-C raises an async exception.
          | Just (_ :: SomeAsyncException) <- fromException ex -> throwIO ex
          | otherwise -> outputException out outMode "Running Failed" ex
        Right val ->
          outputLnS out $ applyStyle outMode styleBold (showString "Result: ") . shows val
      pure $ isRight result

outputException :: (Exception e) => OutputHandle -> OutputMode -> String -> e -> IO ()
outputException h m s e =
  outputLnS h $ header . showChar '\n' . showString (displayException e)
  where
    header =
      applyStyle m (styleBold . styleFG ANSI.Red) $
        showString "===== " . showString s . showString " ====="
