{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AlgST.Main (main) where

import AlgST.Benchmark qualified as Bench
import AlgST.Builtins
import AlgST.CommandLine
import AlgST.Driver (Settings (..))
import AlgST.Driver qualified as Driver
import AlgST.Driver.Output
import AlgST.Driver.Sources
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
import AlgST.Util.SourceManager
import Control.Category ((>>>))
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.DList.DNonEmpty qualified as DNE
import Data.Either
import Data.Foldable
import Data.Function
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Traversable
import Data.Traversable.WithIndex
import Main.Utf8
import Prettyprinter qualified as Pr
import Prettyprinter.Render.Terminal qualified as Pr
import System.Exit
import System.FilePath qualified as FP
import System.IO

main :: IO ()
main = withUtf8 do
  runOpts <- getOptions
  withOutput (optsOutputSettings runOpts) stderr \outputHandle -> runSourcesT do
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
        checkSources runOpts outputHandle src
          >>= maybe (liftIO exitFailure) pure

    -- Use the checked results to answer any queries.
    -- But first, turn all query strings into buffers so we can refer to them
    -- in error messages.
    let addQueryBuffer name q = do
          let buffer = encodedBuffer name q
          addBuffer buffer
          pure (buffer, q)
    bufferQueries <- for (optsQueries runOpts) $ itraverse \case
      QueryTySynth _ -> addQueryBuffer "--type"
      QueryKiSynth _ -> addQueryBuffer "--kind"
      QueryNF _ -> addQueryBuffer "--nf"
    queriesGood <- answerQueries outputHandle bufferQueries checkResult

    -- If benchmarks were requested run them now.
    benchGood <- liftIO case optsBenchmarksOutput runOpts of
      Nothing -> pure True
      Just fp -> Bench.run outputHandle fp checkResult

    -- Run the interpreter if requested.
    runGood <- liftIO do
      if optsDoEval runOpts
        then runInterpret runOpts outputHandle checkResult
        else pure True

    liftIO $ when (not queriesGood || not benchGood || not runGood) do
      exitFailure

checkSources ::
  RunOpts ->
  OutputHandle ->
  Source ->
  SourcesIO (Maybe (HashMap ModuleName (Driver.Result Tc)))
checkSources runOpts outH mainSource = do
  mainSource <- case mainSource of
    SourceFile fp -> do
      buffer <- addFile fp
      pure (Just buffer)
    SourceStdin -> do
      -- If the input comes from the terminal and either of the output
      -- streams goes to the terminal we output a separating newline.
      stdinTerm <- liftIO $ hIsTerminalDevice stdin
      stdoutTerm <- liftIO $ hIsTerminalDevice stdout
      stderrTerm <- liftIO $ hIsTerminalDevice stderr
      let termOut
            | stdinTerm && stdoutTerm = Just stdout
            | stdinTerm && stderrTerm = Just stderr
            | otherwise = Nothing
      buffer <- addStdin
      liftIO $ for_ termOut \h -> hPutChar h '\n'
      pure (Just buffer)
    SourceMain ->
      -- We expect the driver to find the Main module through its usual
      -- module lookup mechanism.
      pure Nothing

  let driverSettings =
        maybe id (Driver.addModuleSource MainModule) mainSource $
          Driver.defaultSettings
            { driverSequential = optsDriverSeq runOpts,
              driverQuietProgress = optsQuiet runOpts,
              driverVerboseDeps = optsDriverDeps runOpts,
              driverVerboseSearches = optsDriverModSearch runOpts,
              driverSearchPaths = FP.normalise <$> optsDriverPaths runOpts,
              driverOutputHandle = outH
            }

  mcheckResult <- Driver.runComplete driverSettings
  when (not (optsQuiet runOpts)) do
    outputDoc outH \wd -> Pr.layoutPretty (Pr.LayoutOptions wd) case mcheckResult of
      Just _ -> Pr.annotate (Pr.bold <> Pr.color Pr.Green) "Success."
      Nothing -> Pr.annotate (Pr.bold <> Pr.color Pr.Red) "Failed."
  pure $ Driver.compactResults <$> mcheckResult

answerQueries ::
  OutputHandle ->
  [Query (Buffer, String)] ->
  HashMap ModuleName (Driver.Result Tc) ->
  SourcesIO Bool
answerQueries out queries checkResult = do
  and <$> for queries \case
    QueryTySynth (buf, s) ->
      parseRename P.parseExpr buf tysynth
        & fmap showTySynth
        & printResult "--type" s
    QueryKiSynth (buf, s) ->
      parseRename P.parseType buf (fmap snd . Tc.kisynth)
        & fmap Pr.viaShow
        & printResult "--kind" s
    QueryNF (buf, s) ->
      parseRename P.parseType buf (Tc.kisynth >=> Tc.normalize . fst)
        & fmap Pr.viaShow
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

    showTySynth = either Pr.viaShow \(t, tNF) ->
      Pr.vcat
        [ Pr.annotate Pr.bold "[SYN]" Pr.<+> Pr.viaShow t,
          Pr.annotate Pr.bold " [NF]" Pr.<+> Pr.viaShow tNF
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

    printResult :: String -> String -> Either D.Errors (Pr.Doc Pr.AnsiStyle) -> SourcesIO Bool
    printResult heading src = \case
      Left errs -> do
        diag <- buildBaseDiagnostic errs
        outputDoc out \wd -> D.layoutDiagnostic D.WithUnicode wd diag
        pure False
      Right body -> do
        outputDoc out \wd -> do
          let doc =
                Pr.vcat
                  [ Pr.annotate (Pr.color Pr.Cyan) . Pr.hsep $
                      [ Pr.annotate Pr.bold (Pr.pretty heading),
                        Pr.pretty (truncateSource src)
                      ],
                    body
                  ]
          Pr.layoutPretty (Pr.LayoutOptions wd) (Pr.line <> doc)
        pure True

    truncateSource :: String -> String
    truncateSource =
      lines >>> \case
        [] -> ""
        [ln] -> Util.truncate' 60 "..." ln
        ln : _ -> take 60 ln ++ "..."

runInterpret ::
  RunOpts -> OutputHandle -> HashMap ModuleName (Driver.Result Tc) -> IO Bool
runInterpret opts out checkedModules = do
  let mmainName = do
        HM.lookup MainModule checkedModules
          >>= Driver.lookupRenamed MainFunction
  outputStrLn out ""
  case mmainName of
    Nothing -> do
      outputError out "No ‘main’ to run."
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
          | otherwise -> outputException out "Running Failed" ex
        Right val ->
          outputDoc out \wd -> Pr.layoutPretty (Pr.LayoutOptions wd) do
            Pr.annotate Pr.bold "Result:" Pr.<+> Pr.viaShow val
      pure $ isRight result

outputException :: (Exception e) => OutputHandle -> String -> e -> IO ()
outputException h s e = outputDoc h \wd -> Pr.layoutPretty (Pr.LayoutOptions wd) do
  let header = Pr.annotate (Pr.bold <> Pr.color Pr.Red) $ "=====" Pr.<+> Pr.pretty s Pr.<+> "====="
  Pr.vcat [header, Pr.pretty (displayException e)]
