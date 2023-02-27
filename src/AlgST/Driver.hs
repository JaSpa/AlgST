{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module AlgST.Driver
  ( -- * Driver monad
    Driver,
    runDriver,
    runComplete,

    -- * Settings
    Settings (..),
    Source,
    defaultSettings,
    setSearchPaths,
    addSearchPathFront,
    addSearchPathBack,
    addModuleSource,
    addModuleSourceIO,

    -- * Actions
    Result (..),
    ResultExtra
      ( RenameExtra,
        rxRenameEnv,
        rxModuleMap,
        CheckExtra,
        cxRename,
        cxContext
      ),
    everything,

    -- ** Parsing
    parseMainModule,
    parseAllModules,

    -- ** Renaming
    renameAll,

    -- ** Type checking
    checkAll,

    -- ** Interpreting results
    HasModuleMap (..),
    HasRenameExtra (..),
    lookupRenamed,
    compactResults,
    resultEvalEnvironment,
    mergedResultEvalEnvironment,
  )
where

import AlgST.Builtins
import AlgST.Driver.Dependencies
import AlgST.Driver.Output
import AlgST.Interpret qualified as I
import AlgST.Parse.Parser qualified as P
import AlgST.Parse.Phase (Parse)
import AlgST.Rename (Rn)
import AlgST.Rename qualified as Rn
import AlgST.Rename.Fresh (runFresh)
import AlgST.Syntax.Module
import AlgST.Syntax.Name
import AlgST.Syntax.Pos
import AlgST.Typing qualified as Tc
import AlgST.Typing.Phase
import AlgST.Util (plural)
import AlgST.Util.Error
import AlgST.Util.ErrorMessage
import AlgST.Util.Output
import Algebra.Graph.AdjacencyMap.Algorithm qualified as G (Cycle)
import Control.Applicative
import Control.Category ((>>>))
import Control.Exception
import Control.Monad.Cont
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.ST (RealWorld)
import Control.Monad.Trans.Maybe
import Control.Monad.Validate
import Control.Scheduler hiding (Scheduler, traverse_)
import Control.Scheduler qualified as S
import Data.Foldable
import Data.Function
import Data.Functor
import Data.HashMap.Lazy qualified as HML
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.IORef
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Singletons
import Lens.Family2 (view, (+~), (.~))
import System.Directory (getCurrentDirectory)
import System.FilePath
import System.IO
import System.IO.Error

data Settings = Settings
  { driverSources :: !(HashMap ModuleName Source),
    driverSearchPaths :: !(Seq FilePath),
    driverQuietProgress :: !Bool,
    driverVerboseSearches :: !Bool,
    driverVerboseDeps :: !Bool,
    driverDebugOutput :: !Bool,
    driverSequential :: !Bool,
    driverOutputMode :: !OutputMode,
    driverOutputHandle :: !OutputHandle
  }
  deriving stock (Show)

data DriverState = DriverState
  { driverSettings :: !Settings,
    driverErrors :: !(IORef Bool),
    -- | Associates each module with the primary file path.
    --
    -- Used to map error messages from modules back to file paths.
    driverModules :: !(IORef (HashMap ModuleName FilePath))
  }

-- | Source code annotated with the 'FilePath' it originated from.
type Source = (FilePath, String)

-- | Schedulers in the driver run in the 'RealWorld'. Computations don't return
-- meaningfull results but update shared data structures.
type Scheduler = S.Scheduler RealWorld ()

type DepsTracker = IORef (DepsGraph MaybeCyclic (Result Parse))

newtype Driver a = Driver {unDriver :: ReaderT DriverState IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

runComplete :: Settings -> IO (Maybe (DepsTree (Maybe (Result Tc))))
runComplete settings = runDriver settings everything

runDriver :: Settings -> Driver a -> IO (Maybe a)
runDriver driverSettings m = do
  driverErrors <- newIORef False
  driverModules <- newIORef (initialDriverModules driverSettings)
  let st = DriverState {..}
  a <-
    runDriverSt st m `finally` do
      clearSticky $ driverOutputHandle driverSettings
  hasError <- readIORef driverErrors
  pure $ a <$ guard (not hasError)

-- | Extracts the initial 'driverModules' entries from 'driverSources'.
initialDriverModules :: Settings -> HM.HashMap ModuleName FilePath
initialDriverModules = driverSources >>> fmap fst

runDriverSt :: DriverState -> Driver a -> IO a
runDriverSt dst (Driver m) = runReaderT m dst

askState :: Driver DriverState
askState = Driver ask

asksState :: (DriverState -> a) -> Driver a
asksState = Driver . asks

defaultSettings :: Settings
defaultSettings =
  Settings
    { driverSources = mempty,
      driverSearchPaths = mempty,
      driverQuietProgress = False,
      driverVerboseSearches = False,
      driverVerboseDeps = False,
      driverDebugOutput = False,
      driverSequential = False,
      driverOutputMode = Plain,
      driverOutputHandle = nullHandle
    }

-- | Insert a module search path at the front.
--
-- Search paths a traversed front to back to locate a module source.
addSearchPathFront :: FilePath -> Settings -> Settings
addSearchPathFront fp s = s {driverSearchPaths = fp :<| driverSearchPaths s}

-- | Insert a module search path at the back.
--
-- Search paths a traversed front to back to locate a module source.
addSearchPathBack :: FilePath -> Settings -> Settings
addSearchPathBack fp s = s {driverSearchPaths = driverSearchPaths s :|> fp}

-- | Replace the moudle search paths with the given list.
--
-- See 'addSearchPathFront' and 'addSearchPathBack' for adding entries to the
-- existing list.
setSearchPaths :: [FilePath] -> Settings -> Settings
setSearchPaths paths s = s {driverSearchPaths = Seq.fromList paths}

-- | Register the source code for the given module. A directly registered
-- module has preference over a module found at a search path. The given file
-- path is used for error messages only.
addModuleSource :: ModuleName -> FilePath -> String -> Settings -> Settings
addModuleSource name fp src settings =
  settings {driverSources = HM.insert name (fp, src) (driverSources settings)}

-- | Read the source code for the given module from the file system and
-- register it using 'addModuleSource'.
addModuleSourceIO :: ModuleName -> FilePath -> Settings -> IO Settings
addModuleSourceIO name fp settings = do
  src <- readFile fp
  pure $ addModuleSource name fp src settings

-- | Checks if the driver is in sequential mode ('driverSequential'). If so the
-- returned computation strategy will be 'Seq', otherwise it will be the given
-- strategy.
askStrategy :: Driver S.Comp
askStrategy = do
  isSeq <- asksState $ driverSettings >>> driverSequential
  pure $! if isSeq then Seq else Par

parScheduled_ :: (S.Scheduler RealWorld a -> Driver b) -> Driver ()
parScheduled_ m = do
  strat <- askStrategy
  Driver . withScheduler_ strat $ unDriver . m

addTask :: S.Scheduler RealWorld a -> Driver a -> Driver ()
addTask scheduler m = do
  env <- askState
  liftIO $ scheduleWork scheduler $ runDriverSt env m

everything :: Driver (DepsTree (Maybe (Result Tc)))
everything = parseMainModule >>= renameAll >>= checkAll

data family ResultExtra x

data Result x = Result
  { resultModule :: Module x,
    resultExtra :: ResultExtra x
  }

data instance ResultExtra Parse = ParseExtra
  { pxImports :: [Located (Import ModuleName)],
    pxGlobals :: Rn.ModuleMap,
    pxRename :: Rn.RenameEnv -> Either DErrors Rn.RenameExtra
  }

parsedModuleResult :: Result Parse -> P.ParsedModule
parsedModuleResult = P.ParsedModule <$> pxImports . resultExtra <*> resultModule

-- | Like 'parseAllModules' but starting from 'MainModule'.
parseMainModule :: Driver (DepsTree (Result Parse))
parseMainModule = parseAllModules MainModule

-- | Parse the given module and all dependencies. The result does not contain
-- any modules where errors occured.
parseAllModules ::
  ModuleName -> Driver (DepsTree (Result Parse))
parseAllModules firstName = do
  depsRef <- liftIO $ newIORef emptyDepsGraph
  (outProgress, _) <- askOutputProgress
  counter <- newCounter outProgress do
    zeroCounter & counterTitleL .~ "Parsing..."
  parScheduled_ $ \scheduler -> do
    parseModule scheduler depsRef counter mempty firstName
  finalDeps <- liftIO $ readIORef depsRef

  let (acyclicDeps, cycles) = removeCycles finalDeps
  (outVDeps, mode) <- askOutputVerbose driverVerboseDeps
  let header :: String -> ShowS
      header s =
        applyStyle mode styleBold $
          showString "== " . showString s . showString " ==\n"
  let showDeps :: String -> DepsGraph cycles a -> ShowS
      showDeps title dg =
        header title . showString (exportTextual dg) . showChar '\n'
  let d1 = showDeps "Dependencies" finalDeps
  let d2 = showDeps "Acyclic Dependencies" acyclicDeps
  outputS outVDeps $ if null cycles then d1 else d1 . d2

  for_ cycles $
    cycleError >>> \case
      Left (fp, diag) -> reportErrors fp [diag]
      Right diag -> reportErrors "" [diag]
  pure acyclicDeps

-- | Tries to locate the source code for the module with the given name using
-- the drivers settings (see 'Settings', 'driverSources', 'driverSearchPaths').
--
-- When the code was located successfully it is submitted for parsing using the
-- given scheduler (see 'parseModuleNow'). When locating the module's source
-- code fails an error is emitted.
parseModule ::
  -- | Where to schedule parsing of the module.
  Scheduler ->
  -- | Where to note down the inter-module dependencies.
  DepsTracker ->
  -- | Keeps track of the progress for user-friendly output.
  Counter ->
  -- | Where this module was imported from. Used only for error messages.
  ImportLocation ->
  -- | Name of the module.
  ModuleName ->
  Driver ()
parseModule scheduler depsRef progress iloc name = do
  counterUpdate progress $ counterOverallL +~ 1
  mmodInfo <- findModule name
  case mmodInfo of
    Nothing -> do
      counterUpdate progress $ counterFinishedL +~ 1
      reportErrors (importLocPath iloc) [missingModuleError (pos iloc) name]
    Just (fp, src) -> do
      addTask scheduler $ parseModuleNow scheduler depsRef progress name fp src

-- | Immediately begin parsing of the provided module. Any unparsed
-- dependencies are scheduled for parsing using the provided scheduler.
parseModuleNow ::
  -- | Where to schedule the dependent unparsed modules.
  Scheduler ->
  -- | Where to note down the inter-module dependencies.
  DepsTracker ->
  -- | Keeps track of the progress for user-friendly output.
  Counter ->
  -- | Name of the module.
  ModuleName ->
  -- | Path to the code of the module. Used only for error messages.
  FilePath ->
  -- | Source code of the module.
  String ->
  Driver ()
parseModuleNow scheduler depsRef progress moduleName modulePath moduleSource =
  wrapCounter progress ("Parsing " ++ unModuleName moduleName) $
    case P.runParser P.parseModule moduleSource of
      Left errs -> do
        reportErrors modulePath errs
      Right parsed -> do
        let (globals, rename) =
              Rn.renameModuleExtra moduleName $ P.parsedModule parsed
        let parseResult =
              Result (P.parsedModule parsed) $
                ParseExtra
                  { pxImports = P.parsedImports parsed,
                    pxGlobals = globals,
                    pxRename = rename
                  }
        newDeps <- noteDependencies depsRef moduleName modulePath parseResult
        traverse_ (uncurry $ parseModule scheduler depsRef progress) newDeps

data instance ResultExtra Rn.Rn = RenameExtra
  { rxRenameEnv :: Rn.RenameEnv,
    rxModuleMap :: Rn.ModuleMap
  }

renameAll ::
  DepsGraph Acyclic (Result Parse) ->
  Driver (DepsTree (Maybe (Result Rn)))
renameAll dg = do
  (outProgress, _) <- askOutputProgress
  counter <- newCounter outProgress do
    zeroCounter & counterOverallL .~ depsGraphSize dg
  parStrat <- askStrategy
  traverseVerticesPar parStrat (renameModule counter) dg
  where
    renameModule counter name parseRes = runMaybeT do
      wrapCounter counter ("Resolving " ++ unModuleName name) do
        -- Resolve imports. Abort, if any dependency failed to parse.
        let findImport n = pxGlobals . resultExtra <$> lookupVertex n dg
        imports <-
          MaybeT
            . pure
            . P.resolveImports findImport
            $ parsedModuleResult parseRes
        -- Run the renamer on this module.
        let doRename = do
              baseEnv <- runValidate $ Rn.foldImportedRenameEnv imports
              let fullEnv = builtinsEnv <> baseEnv
              Rn.RenameExtra go <- pxRename (resultExtra parseRes) fullEnv
              (,fullEnv) <$> go pure
        case doRename of
          Left errs -> do
            lift $ reportModuleErrors name errs
            empty
          Right (rnMod, env) -> do
            let thisEnv =
                  Rn.importAllEnv
                    ZeroPos
                    name
                    (pxGlobals (resultExtra parseRes))
                    emptyModuleName
            let extra =
                  RenameExtra
                    { rxRenameEnv = env <> thisEnv,
                      rxModuleMap = pxGlobals (resultExtra parseRes)
                    }
            pure $ Result rnMod extra

data instance ResultExtra Tc = CheckExtra
  { cxRename :: ResultExtra Rn,
    cxContext :: Tc.CheckContext
  }

checkContext :: Result Tc -> Tc.CheckContext
checkContext = resultExtra >>> \(CheckExtra _ c) -> c

checkAll ::
  DepsGraph Acyclic (Maybe (Result Rn)) ->
  Driver (DepsTree (Maybe (Result Tc)))
checkAll dg = do
  (outProgress, _) <- askOutputProgress
  counter <- newCounterStart outProgress do
    zeroCounter
      & counterOverallL .~ depsGraphSize dg
      & counterTitleL .~ "Checking..."

  strat <- askStrategy
  traverseTreePar strat dg \deps name mRn -> do
    let depCtxt = mconcat . fmap checkContext <$> traverse snd deps
    let mcheck = check name <$> join mRn <*> depCtxt
    join <$> maybeCounter counter "Checking" name mcheck id
  where
    check :: ModuleName -> Result Rn -> Tc.CheckContext -> Driver (Maybe (Result Tc))
    check name rn depCtxt = do
      -- Since we start a new 'Fresh' session for the module (previous one was
      -- during renaming) we have to use a new unique module name to guarantee
      -- uniqueness of fresh variables.
      let freshModule = ModuleName $ "tc:" ++ unModuleName name
      case runFresh freshModule $ runValidateT $ doCheck depCtxt $ resultModule rn of
        Left errs -> do
          reportModuleErrors name $ Tc.runErrors errs
          pure Nothing
        Right (checked, ctxt) -> do
          pure $ Just $ Result checked $ CheckExtra (resultExtra rn) ctxt
    doCheck depCtxt m =
      Tc.checkWithModule
        (builtinsModuleCtxt <> depCtxt)
        m
        (\runTypeM checked -> (checked,) <$> runTypeM Tc.extractCheckContext)

missingModuleError :: Pos -> ModuleName -> Diagnostic
missingModuleError loc name = PosError loc [Error "Cannot locate module", Error name]
{-# NOINLINE missingModuleError #-}

-- | Looks for a module with the given name. If successfull this returns the
-- modules primary source file and its contents.
--
-- The source file is noted in 'driverModules'. If 'driverVerboseSearches' is
-- 'True' additional debug messages will be output during the search.
findModule :: ModuleName -> Driver (Maybe (FilePath, String))
findModule name = flip runContT pure do
  (outVSearches, _) <- lift $ askOutputVerbose driverVerboseSearches
  let searchMsg = outputStrLn outVSearches
  let !verbose = isNullHandle outVSearches
  liftIO $ searchMsg $ "Locating module ‘" ++ unModuleName name ++ "’"

  -- Check if it is a known virtual module.
  virtual <- lift $ asksState $ driverSettings >>> driverSources >>> HM.lookup name
  for_ virtual \res -> do
    -- It is. Early exit.
    liftIO $ searchMsg "++ found as a virtual module"
    ContT $ const $ pure $ Just res

  -- Not a virtual module. Look through the search paths instead.
  env <- lift askState
  let paths = driverSearchPaths $ driverSettings env
  let npaths = show (length paths) ++ plural paths " search path" " search paths"
  liftIO $ searchMsg $ ".. not a virtual module, looking through " ++ npaths

  -- Check all search paths.
  let annotResult m
        | verbose =
            try m >>= \case
              Left (e :: IOError) ->
                searchMsg (".. ⮑  ✗ " ++ displayException e) *> throwIO e
              Right r ->
                searchMsg ".. ⮑  ✔" $> r
        | otherwise = m
  let tryRead dir = do
        let fp = dir </> modulePath name
        searchMsg $ ".. ? " ++ fp
        (fp,) <$> annotResult (readFile' fp)
  res <- liftIO . tryIOError . asum . fmap tryRead $ paths

  -- Remeber a successfull result in `driverModules`.
  lift $ for_ res \(fp, _) -> do
    pathsRef <- asksState driverModules
    liftIO $ atomicModifyIORef' pathsRef \hm ->
      (HM.insert name fp hm, ())

  -- Summarize the result.
  liftIO case res of
    Left _ -> do
      searchMsg $ ".. " ++ npaths ++ " enumerated"
      searchMsg $ "++ Module ‘" ++ unModuleName name ++ "’ not found"
      pure Nothing
    Right found@(fp, _) -> do
      searchMsg $ "++ Module ‘" ++ unModuleName name ++ "’ found at " ++ fp
      pure $ Just found

-- | Register the import dependencies for the given module and returns the set
-- of modules yet to be parsed.
noteDependencies ::
  DepsTracker ->
  -- | Name of the module containing the imports.
  ModuleName ->
  -- | Filepath of the containing module. Used at a later stage for reporting
  -- cyclic dependency errors.
  FilePath ->
  -- | The parsed module from which to update the dependency graph.
  Result Parse ->
  Driver [(ImportLocation, ModuleName)]
noteDependencies depsRef name fp parsed = do
  let depList =
        [ (ImportLocation (p @- fp), target)
          | p :@ Import {importTarget = target} <-
              pxImports $ resultExtra parsed
        ]
  -- Update the dependency graph. This step also signals to any other workers
  -- that this worker will be responsible for delegating parsing of any
  -- unparsed dependencies.
  oldDeps <- liftIO $ atomicModifyIORef' depsRef \deps -> do
    let deps' = insertModule name parsed deps
    let add (iloc, target) = insertDependency iloc (name `DependsOn` target)
    (foldl' (flip add) deps' depList, deps)
  -- Return the list of unparsed dependencies. Those are the ones not appearing
  -- as vertices in `oldDeps`.
  let isUnparsed (_, m) = m /= name && not (depsMember m oldDeps)
  let newDeps = filter isUnparsed depList

  (outVDeps, _) <- askOutputVerbose driverVerboseDeps
  outputStrLn outVDeps do
    let !nOverall = length depList
        !nNew = length newDeps
    let ln1 =
          [ "Module ‘" ++ unModuleName name ++ "’ has",
            show nOverall,
            plural depList "dependency" "dependencies."
          ]
    let ln2 =
          [ "\n..",
            show nNew,
            "new,",
            show (nOverall - nNew),
            "already parsed"
          ]
    unwords ln1 ++ unwords ln2

  pure newDeps

cycleError :: G.Cycle (ModuleName, ImportLocation) -> Either (FilePath, Diagnostic) Diagnostic
cycleError ((m, iloc) :| []) = Left (importLocPath iloc, err)
  where
    err =
      PosError
        (pos iloc)
        [Error "Module", Error m, Error "imports itself."]
cycleError ((m0, iloc0) :| ms) =
  Right . unlocatedError . List.intercalate [ErrLine] $
    [Error "Cycle in module imports:"]
      : step "   Module" m0 iloc0
      ++ concatMap (uncurry $ step "   imports") ms
      ++ [[Error "   imports", Error m0]]
  where
    step s m iloc =
      [ [Error s, Error m],
        [ Error . MsgTag $
            "     at "
              ++ (if null (importLocPath iloc) then "«unknown»" else importLocPath iloc)
              ++ ":"
              ++ show (pos iloc)
        ]
      ]

setError :: Driver ()
setError = do
  ref <- asksState driverErrors
  liftIO $ writeIORef ref True

-- | Report the errors to the user.
reportErrors :: Foldable f => FilePath -> f Diagnostic -> Driver ()
reportErrors fp diags
  | null diags =
      pure ()
  | otherwise = do
      setError
      (handle, mode) <- askOutputDiags
      cwd <- liftIO getCurrentDirectory
      let fpShort = makeRelative cwd fp
      let errs = renderErrors' (Just 10) mode fpShort (toList diags)
      outputS handle $ errs . showChar '\n'

-- | Like 'reportErrors' but the module's file path is looked up inside
-- 'driverModules'. An unknown module results in an empty 'FilePath'.
reportModuleErrors :: Foldable f => ModuleName -> f Diagnostic -> Driver ()
reportModuleErrors m diags = do
  pathMap <- liftIO . readIORef =<< asksState driverModules
  let mpath = HM.lookup m pathMap
  reportErrors (fold mpath) diags

-- | Returns the drivers 'OutputHandle'.
--
-- This function returns the actual 'OutputHandle' unconditionally, as opposed
-- to 'askOutputVerbose' or 'askOutputProgress'. We never want to hide
-- diagnostic messages.
askOutputDiags :: Driver (OutputHandle, OutputMode)
askOutputDiags = asksState do
  (,)
    <$> driverOutputHandle . driverSettings
    <*> driverOutputMode . driverSettings

-- | Either returns the drivers 'OutputHandle' or a 'nullHandle', depending
-- wether the given function returns 'True'.
--
-- For example
--
-- @
-- askOutputVerbose 'driverVerboseSearches'
-- @
--
-- will return the associated handle if verbose searches are enabled.
askOutputVerbose :: (Settings -> Bool) -> Driver (OutputHandle, OutputMode)
askOutputVerbose pred = asksState \DriverState {driverSettings = settings} ->
  ( if pred settings
      then driverOutputHandle settings
      else nullHandle,
    driverOutputMode settings
  )

-- | Returns the output handle to be used for progress updates.
--
-- This returns a 'nullHandle' if the 'driverQuietProgress' is enabled.
askOutputProgress :: Driver (OutputHandle, OutputMode)
askOutputProgress = askOutputVerbose (not . driverQuietProgress)

maybeCounter ::
  MonadIO m =>
  Counter ->
  String ->
  ModuleName ->
  Maybe a ->
  (a -> m b) ->
  m (Maybe b)
maybeCounter c _ _ Nothing _ = do
  counterUpdate c $ counterFinishedL +~ 1
  pure Nothing
maybeCounter c title name (Just a) f = do
  let fullTitle = title ++ ' ' : unModuleName name ++ "..."
  Just <$> wrapCounter c fullTitle (f a)

class HasModuleMap x where
  getModuleMap :: ResultExtra x -> Rn.ModuleMap

instance HasModuleMap Parse where
  getModuleMap = pxGlobals

instance HasModuleMap Rn where
  getModuleMap = rxModuleMap

instance HasModuleMap Tc where
  getModuleMap = cxRename >>> getModuleMap

class HasRenameExtra x where
  getRenameExtra :: ResultExtra x -> ResultExtra Rn

instance HasRenameExtra Rn where
  getRenameExtra = id

instance HasRenameExtra Tc where
  getRenameExtra (CheckExtra rn _) = rn

lookupRenamed ::
  (HasModuleMap x, SingI scope) =>
  Unqualified ->
  Result x ->
  Maybe (Name Resolved scope)
lookupRenamed n =
  resultExtra
    >>> getModuleMap
    >>> view (scopeL . Rn._TopLevels)
    >>> HM.lookup n

compactResults :: DepsGraph cycles (Maybe a) -> HashMap ModuleName a
compactResults = depsVertices >>> HML.mapMaybe id

resultEvalEnvironment :: Result Tc -> I.Env
resultEvalEnvironment = I.programEnvironment . resultModule

mergedResultEvalEnvironment :: Foldable f => f (Result Tc) -> I.Env
mergedResultEvalEnvironment = foldMap resultEvalEnvironment
