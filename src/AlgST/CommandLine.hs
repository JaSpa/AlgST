{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module AlgST.CommandLine
  ( getOptions,
    RunOpts (..),
    Source (..),
    Query (..),
    actionSource,
  )
where

import AlgST.Interpret qualified as I
import AlgST.Util.Output
import Control.Applicative
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Numeric.Natural (Natural)
import Options.Applicative qualified as O

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
        [ O.long "main",
          O.help "Look for module ‘Main’ in the search path.",
          O.hidden
        ]

    decideInput "-" = SourceStdin
    decideInput fp = SourceFile fp

data Query
  = QueryTySynth !String
  | QueryKiSynth !String
  | QueryNF !String
  deriving (Show)

actionSource :: Query -> String
actionSource = \case
  QueryTySynth s -> s
  QueryKiSynth s -> s
  QueryNF s -> s

queryParser :: O.Parser Query
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
    optsQueries :: ![Query],
    optsDoEval :: !Bool,
    optsDebugEval :: !Bool,
    optsBufferSize :: !Natural,
    optsDriverPaths :: !(Seq FilePath),
    optsDriverSeq :: !Bool,
    optsDriverDeps :: !Bool,
    optsDriverModSearch :: !Bool
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
  optsDriverSeq <- driverSeqParser
  optsDriverDeps <- driverDepsParser
  optsDriverModSearch <- driverModSearchParser
  pure RunOpts {..}

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

getOptions :: IO RunOpts
getOptions =
  O.execParser $
    O.info
      (optsParser <**> O.helper)
      (mconcat parserInfo)
  where
    parserInfo =
      [ O.header
          "AlgST - frontend, typechecker and interpreter for Algebraic \
          \Session Types."
      ]
