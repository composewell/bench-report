{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module BenchRunner
    ( mainWith
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.Foldable (for_)
import Control.Monad (when, unless, void)
import Control.Monad.IO.Class (MonadIO(..))
import Utils.QuasiQuoter (line)
import Control.Monad.Trans.State.Strict (execStateT, gets, modify)
import Data.List (intersperse, isSuffixOf)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeFileName, takeDirectory, (</>))
import Data.Function ((&))
import BenchShow.Internal.Common (GroupStyle(..))
import Data.Map (Map)

import qualified Options.Applicative as OptParse
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import qualified Streamly.Coreutils.FileTest as Test
import qualified Data.Map as Map
import qualified BenchReport

import Utils
import BuildLib

import Prelude hiding (compare)
import Options.Applicative hiding (Parser)
import Options.Applicative.Simple

--------------------------------------------------------------------------------
-- CLI
--------------------------------------------------------------------------------

targetsFromString :: String -> Either String [Target]
targetsFromString xs = Right $ map stringToTarget $ words xs

cliOptions :: OptParse.Parser Configuration
cliOptions = do
    Configuration <$> switch (long "dev-build")
        <*> pure (config_GROUP_TARGETS defaultConfig)
        <*> pure (config_COMPARISIONS defaultConfig)
        <*> pure (config_INDIVIDUAL_TARGETS defaultConfig)
        <*> (option
                 (eitherReader targetsFromString)
                 (long "targets"
                      <> value (config_TARGETS defaultConfig)))
        <*> pure (config_TEST_QUICK_MODE defaultConfig)
        <*> pure (config_GHC_VERSION defaultConfig)
        <*> pure (config_BUILD_DIR defaultConfig)
        <*> strOption
              (long "cabal-build-options"
                   <> value (config_CABAL_BUILD_OPTIONS defaultConfig))
        <*> strOption
              (long "with-compiler"
                   <> value (config_CABAL_WITH_COMPILER defaultConfig))
        <*> pure (config_CABAL_EXECUTABLE defaultConfig)
        <*> strOption
              (long "rts-opts" <> value (config_RTS_OPTIONS defaultConfig))
        <*> pure (config_TARGET_EXE_ARGS defaultConfig)
        <*> switch (long "quick")
        <*> switch (long "slow")
        <*> pure (config_USE_GIT_CABAL defaultConfig)
        <*> switch (long "long")
        <*> strOption
              (long "prefix" <> value (config_BENCH_PREFIX defaultConfig))
        <*> strOption
              (long "gauge-args" <> value (config_GAUGE_ARGS defaultConfig))
        <*> pure (config_BENCHMARK_PACKAGE_VERSION defaultConfig)
        <*> switch (long "append")
        <*> switch (long "commit-compare")
        <*> pure (config_BUILD_BENCH defaultConfig)
        <*> pure (config_BENCHMARK_PACKAGE_NAME defaultConfig)
        <*> (words
                 <$> strOption
                       (long "fields"
                            <> value (unwords (config_FIELDS defaultConfig))))
        <*> (read
                 <$> strOption
                       (long "diff-cutoff-percent"
                            <> value
                                  (show
                                       (config_BENCH_CUTOFF_PERCENT
                                            defaultConfig))))
        <*> option
              (eitherReader diffStyleFromString)
              (long "diff-style"
                   <> value (config_BENCH_DIFF_STYLE defaultConfig))
        <*> switch (long "sort-by-name")
        <*> switch (long "graphs")
        <*> switch (long "silent")
        <*> switch (long "raw")
        <*> unswitch (long "no-measure")
        <*> pure (config_DEFAULT_FIELDS defaultConfig)
        <*> pure (config_ALL_FIELDS defaultConfig)
        <*> pure (config_BUILD_FLAGS defaultConfig)
        <*> pure (config_INFINITE_GRP defaultConfig)
        <*> switch (long "compare")
        <*> pure (config_BENCH_SPEED_OPTIONS defaultConfig)
        <*> pure (config_BENCH_RTS_OPTIONS defaultConfig)

    where

    unswitch = flag True False

    diffStyleFromString val =
        case val of
            "absolute" -> Right Absolute
            "multiples" -> Right Multiples
            "percent" -> Right PercentDiff
            x -> Left $ "Unknown diff option: " ++ show x

--------------------------------------------------------------------------------
-- Reporting utility functions
--------------------------------------------------------------------------------

benchOutputFile :: String -> String
benchOutputFile benchName = "charts" </> benchName </> "results.csv"

--------------------------------------------------------------------------------
-- Speed options
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Determine options from benchmark name
--------------------------------------------------------------------------------

benchExecOne :: String -> String -> String -> Context ()
benchExecOne benchExecPath benchName otherOptions = do
    benchSpeedOptions <- gets config_BENCH_SPEED_OPTIONS
    benchRTSOptions <- gets config_BENCH_RTS_OPTIONS
    let benchBaseName = takeFileName benchExecPath
    -- Using no name conversion
    let superQuickOptions = "--stdev 1000000"
        quickerOptions = "--stdev 100"
        localRTSOptions = benchRTSOptions benchBaseName benchName
    quickMode <- gets config_QUICK_MODE
    long_ <- gets config_LONG
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
            if long_
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
        outputDir = takeDirectory outputFile
    liftIO $ createDirectoryIfMissing True outputDir
    liftIO $ run [line| rm -f $outputFile.tmp |]
    benchNameEscaped <-
        liftIO
            $ runUtf8'
                  [line| echo "$benchName" | sed -e 's/\\/\\\\/g' | sed -e 's/"/\\"/g' | sed -e "s/'/'\\\''/g" |]
    liftIO $ run $ [line|
$benchExecPath
  -j 1
  $rtsOptions1
  $streamSizeOpt
  $quickBenchOptions
  $otherOptions
  --csv=$outputFile.tmp
  -p '$$0 == "$benchNameEscaped"'
  || die "Benchmark execution failed."
|]

    -- Convert cpuTime field from picoseconds to seconds
    liftIO $ run $ [line|
awk --version 2>&1 | grep -q "GNU Awk"
  || die "Need GNU awk. [$(which awk)] is not GNU awk."
|]
    liftIO $ run $ [line|
tail -n +2 $outputFile.tmp
    | awk 'BEGIN {FPAT = "([^,]+)|(\"[^\"]+\")";OFS=","} {$$2=$$2/1000000000000;print}'
    >> $outputFile
|]

invokeTastyBench :: String -> String -> String -> Context ()
invokeTastyBench targetProg targetName outputFile = do
    long_ <- gets config_LONG
    benchPrefix <- gets config_BENCH_PREFIX
    gaugeArgs <- gets config_GAUGE_ARGS
    escapedBenchPrefix <-
        liftIO $ runUtf8' [line| echo "$benchPrefix" | sed -e 's/\//\\\//g' |]
    let match =
            if long_
            then [line| -p /$targetName\\/o-1-space/ |]
            else if null benchPrefix
                 then ""
                 else [line| -p /$escapedBenchPrefix/ |]
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
runBenchesComparing _ = undefined

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
    targets <- catIndividuals <$> gets config_TARGETS
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
                        , BenchReport.benchType = Just $
                            if "_cmp" `isSuffixOf` i
                            then BenchReport.Compare i
                            else BenchReport.Standard i
                        }

-------------------------------------------------------------------------------
-- Execution bootstrapping
-------------------------------------------------------------------------------

bootstrap :: Context ()
bootstrap = do
    modify $ \conf -> conf {config_USE_GIT_CABAL = False}
    setCommonVars
    -- XXX Temporarily skipping comparision vars
    modify
        $ \conf ->
              conf
                  { config_CABAL_BUILD_OPTIONS =
                        let cliOpts = config_CABAL_BUILD_OPTIONS conf
                         in [line| --flag fusion-plugin --flag limit-build-mem $cliOpts |]
                  }

postCLIParsing :: Context ()
postCLIParsing = do
    fields <- gets config_FIELDS
    defFields <- gets config_DEFAULT_FIELDS
    when (null fields) $ modify $ \conf -> conf {config_FIELDS = defFields}
    setDerivedVars

printHelpOnArgs :: Context Bool
printHelpOnArgs = do
    targets <- gets config_TARGETS
    when (hasItem (TIndividual "help") targets) $ do
        listTargets
        listTargetGroups
        listComparisions
    fields <- gets config_FIELDS
    allFields <- gets config_ALL_FIELDS
    defFields <- gets config_DEFAULT_FIELDS
    when (hasItem "help" fields) $ liftIO $ do
        putStr "Supported fields: "
        putStrLn $ unwords allFields
        putStr "Default fields: "
        putStrLn $ unwords defFields
    return $ hasItem "help" fields || hasItem (TIndividual "help") targets

-------------------------------------------------------------------------------
-- Determine targets
-------------------------------------------------------------------------------

flagLongSetup :: Context ()
flagLongSetup = do
    isLong <- gets config_LONG
    targets <- gets config_TARGETS
    let targetsStr = unwords $ catIndividuals targets
    when (isLong && not (null targets))
        $ liftIO
        $ die [line| Cannot specify benchmarks [$targetsStr] with --long |]
    when isLong
        $ modify $ \conf -> conf {config_TARGETS = config_INFINITE_GRP conf}

setupTargets :: Context ()
setupTargets = do
    setTargets_ <- setTargets
    modify $ \conf -> conf {config_TARGETS = setTargets_}

postSettingUpTargets :: Context ()
postSettingUpTargets = do
    silent <- gets config_SILENT
    targetsStr <- unwords . catIndividuals <$> gets config_TARGETS
    unless silent
        $ liftIO $ putStrLn [line| "Using benchmark suites [$targetsStr]" |]

-------------------------------------------------------------------------------
-- Build and run targets
-------------------------------------------------------------------------------

buildAndRunTargets :: Context ()
buildAndRunTargets = do
    cabalExecutable <- gets config_CABAL_EXECUTABLE
    buildFlags <- gets config_BUILD_FLAGS
    cabalBuildOptions <- gets config_CABAL_BUILD_OPTIONS
    modify
        $ \conf ->
              conf
                  { config_BUILD_BENCH =
                        [line| $cabalExecutable v2-build $buildFlags $cabalBuildOptions --enable-benchmarks |]
                  }
    measure <- gets config_MEASURE
    targets <- gets config_TARGETS
    when measure $ runMeasurements (catIndividuals targets)

-------------------------------------------------------------------------------
-- Run reports
-------------------------------------------------------------------------------

buildComparisionReslts :: String -> [String] -> Context ()
buildComparisionReslts name constituents = do
    liftIO $ createDirectoryIfMissing True [line| charts/$name |]
    let destFile = [line| charts/$name/results.csv |]
    liftIO $ runVerbose [line| : > $destFile |]
    for_ constituents
        $ \j ->
              liftIO
                  $ runVerbose
                        [line| cat "charts/$j/results.csv" >> $destFile |]

runFinalReports :: Context ()
runFinalReports = do
    compare <- gets config_COMPARE
    targets <- gets config_TARGETS
    comparisions <- gets config_COMPARISIONS
    raw <- gets config_RAW
    let targetsStr = unwords $ catIndividuals targets
        individualTargets = catIndividuals targets
        comparisionTargets = catComparisions targets
    when (not raw) $ runReports individualTargets
    for_ comparisionTargets
        $ \i -> buildComparisionReslts i (comparisions Map.! i)
    when (not raw) $ runReports comparisionTargets
    when compare
        $ do
            dynCmpGrpName <-
                liftIO
                    $ (++ "_cmp")
                    <$> runUtf8' [line| echo "$targetsStr" | sed -e 's/ /_/g' |]
            buildComparisionReslts dynCmpGrpName individualTargets
            when (not raw) $ runReports [dynCmpGrpName]
            liftIO $ runVerbose [line| rm -rf "charts/$dynCmpGrpName" |]

--------------------------------------------------------------------------------
-- Pipeline
--------------------------------------------------------------------------------

runPipeline :: Context ()
runPipeline = do
    bootstrap
    postCLIParsing
    hasHelp <- printHelpOnArgs
    unless hasHelp $ do
         flagLongSetup
         setupTargets
         postSettingUpTargets
         buildAndRunTargets
         runFinalReports

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

mainWith ::
       Map String [String]
    -> [String]
    -> Map String [String]
    -> (String -> String -> Maybe Quickness)
    -> (String -> String -> String)
    -> IO ()
mainWith grpTargets indTargers cmps speedOpts rtsOpts = do
    (conf, ()) <-
        simpleOptions
            "0.0.0"
            "bench"
            "A helper tool for benchmarking"
            cliOptions
            empty
    void
        $ execStateT runPipeline
        $ conf
              { config_GROUP_TARGETS = grpTargets
              , config_INDIVIDUAL_TARGETS = indTargers
              , config_COMPARISIONS = cmps
              , config_BENCH_SPEED_OPTIONS = speedOpts
              , config_BENCH_RTS_OPTIONS = rtsOpts
              }
