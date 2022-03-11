{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.Foldable (for_)
import Control.Monad (when, unless)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Map (Map)
import Utils.QuasiQuoter (line)
import Control.Monad.Trans.State.Strict (StateT, get, gets, put, modify)
import Data.List (isSuffixOf, nub, sort, intersperse)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeFileName, takeDirectory)
import Data.Function ((&))

import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import qualified Streamly.Coreutils.FileTest as Test
import qualified Data.Map as Map
import qualified BenchReport

import Utils
import BuildLib

--------------------------------------------------------------------------------
-- CLI
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Reporting utility functions
--------------------------------------------------------------------------------

listComparisions :: Context ()
listComparisions = do
    liftIO $ putStrLn "Comparison groups:"
    res <- gets config_COMPARISIONS
    let xs = Map.foldrWithKey (\k_ v_ b -> b ++ [pretty k_ v_]) [] res
    liftIO $ putStr $ unlines xs

    where

    pretty k v = k ++ " [" ++ concat (intersperse ", " v) ++ "]"

benchOutputFile :: String -> String
benchOutputFile benchName = [line| charts/$benchName/results.csv |]

--------------------------------------------------------------------------------
-- Speed options
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Determine options from benchmark name
--------------------------------------------------------------------------------

data Quickness
    = Quicker
    | SuperQuick

benchExecOne :: String -> String -> String -> Context ()
benchExecOne benchExecPath benchName otherOptions = do
    let benchBaseName = takeFileName benchExecPath
    -- Using no name conversion
    let superQuickOptions = "--stdev 1000000"
        quickerOptions = "--stdev 100"
        localRTSOptions = benchRTSOptions benchBaseName benchName
    quickMode <- gets config_QUICK_MODE
    long <- gets config_LONG
    globalRTSOptions <- gets config_RTS_OPTIONS
    let rtsOptions1 = [line| +RTS -T $localRTSOptions $globalRTSOptions -RTS  |]
    let quickBenchOptions =
            if quickMode
            then superQuickOptions
            else case benchSpeedOptions benchBaseName benchName of
                     Nothing -> ""
                     Just Quicker -> quickerOptions
                     Just SuperQuick -> superQuickOptions
    -- Skipping STREAM_LEN as it is just a display trick
    let streamSize =
            if long
            then Just (10000000 :: Int)
            else Nothing
    streamLen <-
        case show <$> streamSize of
            Just size ->
                liftIO
                    $ runUtf8'
                          [line| env LC_ALL=en_US.UTF-8 printf "--stream-size %'.f\n" $size |]
            Nothing -> return ""
    let streamSizeOpt =
            case show <$> streamSize of
                Just size -> [line| --stream-size $size |]
                Nothing -> ""
    liftIO $ putStrLn
        [line| $benchName $rtsOptions1 $streamLen $quickBenchOptions $otherOptions |]

    ----------------------------------------------------------------------------
    -- Run benchmark with options and collect results
    ----------------------------------------------------------------------------

    let outputFile = benchOutputFile benchBaseName
    liftIO $ createDirectoryIfMissing True outputFile
    liftIO $ runVerbose [line| rm -f $outputFile.tmp |]
    benchNameEscaped <-
        liftIO
            $ runUtf8'
                  [line| echo "$benchName" | sed -e 's/\\/\\\\/g' | sed -e 's/"/\\"/g' |]
    liftIO $ runVerbose $ [line|
$benchExecPath
  -j 1
  $rtsOptions1
  $streamSizeOpt
  $quickBenchOptions
  $otherOptions
  --csv=$outputFile.tmp
  -p '$benchName == "'"$benchNameEscaped"'"'
  || die "Benchmark execution failed."
|]

    -- Convert cpuTime field from picoseconds to seconds
    liftIO $ runVerbose $ [line|
awk --version 2>&1 | grep -q "GNU Awk"
  || die "Need GNU awk. [$(which awk)] is not GNU awk."
|]
    liftIO $ runVerbose $ [line|
tail -n +2 $outputFile.tmp
    | awk 'BEGIN {FPAT = "([^,]+)|(\"[^\"]+\")";OFS=","} {$$2=$$2/1000000000000;print}'
    >> $outputFile
|]

    where

    benchRTSOptions = undefined
    benchSpeedOptions = undefined

invokeTastyBench :: String -> String -> String -> Context ()
invokeTastyBench targetProg targetName outputFile = do
    long <- gets config_LONG
    benchPrefix <- gets config_BENCH_PREFIX
    gaugeArgs <- gets config_GAUGE_ARGS
    escapedBenchPrefix <-
        liftIO $ runUtf8' [line| echo "$benchPrefix" | sed -e 's/\//\\\//g' |]
    let match =
            if long
            then "-p /$target_name\\/o-1-space/"
            else if null benchPrefix
                 then ""
                 else "-p /$escapedBenchPrefix/"
    liftIO
        $ runVerbose
              [line| echo "Name,cpuTime,2*Stdev (ps),Allocated,bytesCopied,maxrss" >> $outputFile |]
    benchmarkNames <-
        liftIO
            $ runUtf8 [line| $targetProg -l $match | grep "^All"  |]
            & Stream.toList
    for_ benchmarkNames $ \name -> benchExecOne targetProg name gaugeArgs

runBenchTarget :: String -> String -> String -> Context ()
runBenchTarget packageName component targetName = do
    benchmarkPackageVersion <- gets config_BENCHMARK_PACKAGE_VERSION
    mTargetProg <-
        cabalTargetProg
            [line| $packageName-$benchmarkPackageVersion |]
            component
            targetName
    case mTargetProg of
        Nothing ->
            liftIO $ die [line| Cannot find executable for target $targetName |]
        Just targetProg -> do
            liftIO $ putStrLn "Running executable $targetName ..."
            let outputFile = benchOutputFile targetName
                outputDir = takeDirectory outputFile
            liftIO $ createDirectoryIfMissing True outputDir
            invokeTastyBench targetProg targetName outputFile

runBenchTargets :: String -> String -> [String] -> Context ()
runBenchTargets packageName component targets =
    for_ targets $ runBenchTarget packageName component

runBenchesComparing :: [String] -> Context ()
runBenchesComparing benchList = undefined

backupOutputFile :: String -> Context ()
backupOutputFile benchName = do
    let outputFile = benchOutputFile benchName
    append <- gets config_APPEND
    exists <- liftIO $ Test.test outputFile Test.exists
    when (not append && exists)
        $ liftIO $ run [line| mv -f -v $outputFile $outputFile.prev |]

runMeasurements :: [String] -> Context ()
runMeasurements benchList = do
    for_ benchList backupOutputFile
    commitCompare <- gets config_COMMIT_COMPARE
    buildBench <- gets config_BUILD_BENCH
    benchPackageName <- gets config_BENCHMARK_PACKAGE_NAME
    targets <- gets config_TARGETS
    if commitCompare
    then runBenchesComparing benchList
    else do
        runBuild buildBench benchPackageName "bench" targets
        -- XXX What is target_exe_extra_args here?
        runBenchTargets benchPackageName "b" benchList

runReports :: [String] -> Context ()
runReports benchmarks = do
    silent <- gets config_SILENT
    graphs <- gets config_GRAPH
    sortByName <- gets config_SORT_BY_NAME
    diffStyle <- gets config_BENCH_DIFF_STYLE
    cutOffPercent <- gets config_BENCH_CUTOFF_PERCENT
    fields <- gets config_FIELDS
    for_ benchmarks
        $ \i -> liftIO $ do
              unless silent $ putStrLn [line| Generating reports for $i... |]
              BenchReport.runBenchReport
                  $ BenchReport.defaultOptions
                        { BenchReport.genGraphs = graphs
                        , BenchReport.sortByName = sortByName
                        , BenchReport.fields = fields
                        , BenchReport.diffStyle = diffStyle
                        , BenchReport.cutOffPercent = cutOffPercent
                        }

-------------------------------------------------------------------------------
-- Execution bootstrapping
-------------------------------------------------------------------------------

bootstrap :: Context ()
bootstrap = do
    modify $ \conf -> conf {config_USE_GIT_CABAL = True}
    setCommonVars
    -- XXX Temporarily skipping comparision vars
    modify $ \conf -> conf {config_APPEND = False}
    modify $ \conf -> conf {config_LONG = False}
    modify $ \conf -> conf {config_RAW = False}
    modify $ \conf -> conf {config_SORT_BY_NAME = False}
    modify $ \conf -> conf {config_GRAPH = False}
    modify $ \conf -> conf {config_MEASURE = True}
    modify $ \conf -> conf {config_GAUGE_ARGS = ""}
    modify
        $ \conf ->
              conf
                  { config_CABAL_BUILD_OPTIONS =
                        "--flag fusion-plugin --flag limit-build-mem"
                  }

postCLIParsing :: Context ()
postCLIParsing = do
    fields <- gets config_FIELDS
    defFields <- gets config_DEFAULT_FIELDS
    when (null fields) $ modify $ \conf -> conf {config_FIELDS = defFields}
    setDerivedVars

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = putStrLn "Hello World!"
