{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module AlgST.CommandLine
  ( getOptions,
    RunOpts (..),
    Source (..),
    Query (..),
    queryFlag,
    queryData,
  )
where

import AlgST.Benchmark qualified as Bench
import AlgST.Interpret qualified as I
import AlgST.Util.Output
import Control.Applicative
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Version
import Numeric.Natural (Natural)
import Options.Applicative qualified as O
import Paths_AlgST

data Source
  = SourceFile !FilePath
  | SourceStdin
  | SourceMain
  deriving (Eq, Show)

sourceParser :: O.Parser (Maybe Source)
sourceParser = O.optional $ givePath <|> searchMain
  where
    givePath =
      fmap decideInput . O.strArgument . mconcat $
        [ O.metavar "FILE",
          O.help "Read Main module from FILE. Use ‘-’ to read from standard input."
        ]

    searchMain =
      O.flag' SourceMain . mconcat $
        [ O.long "find-main",
          O.help "Look for module ‘Main’ in the search path.",
          O.hidden
        ]

    decideInput "-" = SourceStdin
    decideInput fp = SourceFile fp

data Query a
  = QueryTySynth a
  | QueryKiSynth a
  | QueryNF a
  deriving (Show)

-- | Returns the flag name for the given kind of query.
queryFlag :: Query a -> String
queryFlag = \case
  QueryTySynth _ -> "--type"
  QueryKiSynth _ -> "--kind"
  QueryNF _ -> "--nf"

-- | Returns the query data @a@ stored inside a @'Query' a@.
queryData :: Query a -> a
queryData = \case
  QueryTySynth a -> a
  QueryKiSynth a -> a
  QueryNF a -> a

queryParser :: O.Parser (Query String)
queryParser = tysynth <|> kisynth <|> nf
  where
    synthHelp x y =
      unwords
        [ "Synthesize the",
          x,
          "of the given",
          y,
          "in the context of the Main module.",
          "Can be repeated."
        ]
    tysynth =
      fmap QueryTySynth . O.strOption . mconcat $
        [ O.long "type",
          O.short 'T',
          O.metavar "EXPR",
          O.help $ synthHelp "type" "expression"
        ]
    kisynth =
      fmap QueryKiSynth . O.strOption . mconcat $
        [ O.long "kind",
          O.short 'K',
          O.metavar "TYPE",
          O.help $ synthHelp "kind" "type"
        ]
    nf =
      fmap QueryNF . O.strOption . mconcat $
        [ O.long "nf",
          O.metavar "TYPE",
          O.help
            "Calculate the normal form of the given type in the context of \
            \the Main module. Can be repeated."
        ]

data RunOpts = RunOpts
  { optsSource :: !(Maybe Source),
    optsOutputMode :: !(Maybe OutputMode),
    optsQuiet :: !Bool,
    optsQueries :: ![Query String],
    optsDoEval :: !Bool,
    optsDebugEval :: !Bool,
    optsBufferSize :: !Natural,
    optsDriverPaths :: !(Seq FilePath),
    optsDriverSeq :: !Bool,
    optsDriverDeps :: !Bool,
    optsDriverModSearch :: !Bool,
    optsBenchmarksOutput :: !(Maybe FilePath)
  }
  deriving (Show)

optsParser :: O.Parser RunOpts
optsParser = do
  optsSource <- sourceParser
  optsDriverPaths <- driverSearchDirs
  optsDoEval <- evalFlagParser
  optsBufferSize <- evalBufSizeParser
  optsDebugEval <- evalVerboseParser
  optsQueries <- many queryParser
  optsOutputMode <- optional modeParser
  optsQuiet <- quietParser
  optsDriverSeq <- driverSeqParser
  optsDriverDeps <- driverDepsParser
  optsDriverModSearch <- driverModSearchParser
  optsBenchmarksOutput <- benchmarksOutParser
  pure RunOpts {..}

benchmarksOutParser :: O.Parser (Maybe FilePath)
benchmarksOutParser
  | Bench.enabled = optional $ O.strOption (O.hidden <> opts)
  | otherwise = O.abortOption disabledError (O.internal <> opts) <*> pure Nothing
  where
    opts =
      mconcat
        [ O.long "bench",
          O.metavar "FILE",
          O.help "Run benchmarks specified in Main module and write results to FILE (CSV)."
        ]
    disabledError =
      O.ErrorMsg "AlgST benchmarker is not enabled in this build."

modeParser :: O.Parser OutputMode
modeParser = plain <|> colorized
  where
    plain =
      O.flag' Plain $
        mconcat
          [ O.long "plain",
            O.short 'p',
            O.help "Output messages without colors.",
            O.hidden
          ]
    colorized =
      O.flag' Colorized $
        mconcat
          [ O.long "colors",
            O.help
              "Output messages with colors even when the output device is \
              \not a terminal.",
            O.hidden
          ]

quietParser :: O.Parser Bool
quietParser =
  O.flag False True . mconcat $
    [ O.long "quiet",
      O.short 'q',
      O.help "Suppress informative messages.",
      O.hidden
    ]

evalFlagParser :: O.Parser Bool
evalFlagParser =
  O.flag False True . mconcat $
    [ O.long "run",
      O.help "Look for a ‘main’ symbol visible from the Main module to run."
    ]

evalVerboseParser :: O.Parser Bool
evalVerboseParser =
  O.flag False True . mconcat $
    [ O.long "eval-verbose",
      O.help "Output verbose messages during evaluation.",
      O.hidden
    ]

evalBufSizeParser :: O.Parser Natural
evalBufSizeParser =
  O.option O.auto . mconcat $
    [ O.long "eval-chan-size",
      O.value (I.evalBufferSize I.defaultSettings),
      O.metavar "N",
      O.help
        "The buffer size of channels when interpreted. ‘0’ specifies fully \
        \synchronous communication, which is the default.",
      O.hidden
    ]

driverSearchDirs :: O.Parser (Seq FilePath)
driverSearchDirs =
  fmap nullDefault . O.many . O.strOption . mconcat $
    [ O.long "search-dir",
      O.short 'I',
      O.metavar "DIR",
      O.help
        "Search the given directory for imported modules. Can be repeated to \
        \search multiple directories in order. If not specfied it defaults to \
        \the working directory."
    ]
  where
    nullDefault [] = Seq.singleton "."
    nullDefault ps = Seq.fromList ps

driverSeqParser :: O.Parser Bool
driverSeqParser =
  driverDebugFlag
    "driver-sequential"
    "Disable concurrency in the driver."

driverDepsParser :: O.Parser Bool
driverDepsParser =
  driverDebugFlag
    "driver-verbose-deps"
    "Enable verbose output during dependency discover, and print the \
    \resulting dependency graph."

driverModSearchParser :: O.Parser Bool
driverModSearchParser =
  driverDebugFlag
    "driver-verbose-modules"
    "Enable verbose output regarding which modules are searched for, where \
    \they are found or why they are not found."

driverDebugFlag :: String -> String -> O.Parser Bool
driverDebugFlag name help =
  O.flag False True $
    O.long name <> O.help help <> O.hidden

versionOption :: O.Parser (a -> a)
versionOption =
  O.infoOption ("algst version " ++ showVersion version) . mconcat $
    [ O.long "version",
      O.short 'V',
      O.help "Show the current version."
    ]

getOptions :: IO RunOpts
getOptions =
  O.execParser $
    O.info
      (optsParser <**> O.helper <**> versionOption)
      (mconcat parserInfo)
  where
    parserInfo =
      [ O.header
          "AlgST - frontend, typechecker and interpreter for Algebraic \
          \Session Types."
      ]
