{-# LANGUAGE CPP #-}

module AlgST.Benchmark where

import AlgST.Driver qualified as Driver
import AlgST.Driver.Output
import AlgST.Syntax.Name
import AlgST.Typing.Phase (Tc)
import Data.HashMap.Internal.Strict (HashMap)

#ifdef ALGST_BENCHMARKER

import AlgST.Syntax.Module
import AlgST.Typing.Align qualified as Typing
import AlgST.Typing.NormalForm qualified as Typing
import Data.Foldable
import Data.HashMap.Strict qualified as HM
import Data.Maybe
import Gauge qualified

-- ALGST_BENCHMARKER
#endif

enabled :: Bool
enabled =
#ifdef ALGST_BENCHMARKER
  True
#else
  False
#endif

-- | Run the benchmarks and write the result to the given file path.
run ::
  OutputHandle ->
  FilePath ->
  HashMap ModuleName (Driver.Result Tc) ->
  IO Bool

#ifdef ALGST_BENCHMARKER

run outH fp modules
  | null benchmarks = do
      let msg = "Benchmarks requested but Main module does not specify any."
      outputError outH msg
      pure False
  | otherwise = do
      -- Truncate the CSV file before running the benchmarks. Gauge only ever appends.
      writeFile fp ""
      -- I would like to have a sticky "Running benchmarks..." at the bottom
      -- which is cleared when the program finishes. However, gauge always
      -- writes something to stdout, which conflicts with our use.
      let benchOpts = Gauge.defaultConfig {Gauge.csvFile = Just fp}
      Gauge.runMode Gauge.DefaultMode benchOpts [] benchmarks
      pure True
  where
    benchmarks = fold do
      res <- HM.lookup MainModule modules
      pure $ mkBench <$> moduleBench (Driver.resultModule res)
    mkBench bench = Gauge.bench (benchName bench ++ " [AlgST]") do
      Gauge.nf (uncurry checkEq) (benchT1 bench, benchT2 bench)
    checkEq t u =
      error "internal error: NF calculation failed during benchmark" `fromMaybe` do
        tNF <- Typing.nf t
        uNF <- Typing.nf u
        pure $ Typing.Alpha tNF == Typing.Alpha uNF

-- ALGST_BENCHMARKER
#else

run _ _ _ = do
  pure False

-- ALGST_BENCHMARKER
#endif
