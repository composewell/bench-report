{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module BenchRunner
    ( mainWith
    )
where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import BenchShow.Internal.Common (GroupStyle(..))
import Control.Monad (when, unless, void)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.List (isSuffixOf)
import Data.Map (Map)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeFileName, takeDirectory, (</>))
import Streamly.Internal.Unicode.String (str)

import qualified BenchReport
import qualified Data.Map as Map
import qualified Options.Applicative as OptParse
import qualified Streamly.Coreutils.FileTest as Test
import qualified Streamly.Internal.Data.Stream.IsStream as Stream

import Utils
import BuildLib

import Prelude hiding (compare)
import Options.Applicative hiding (Parser, str)
import Options.Applicative.Simple hiding (str)

data Configuration =
    Configuration
        { bconfig_RUNNING_DEVBUILD :: Bool
        , bconfig_GROUP_TARGETS :: Map String [String]
        , bconfig_COMPARISONS :: Map String [String]
        , bconfig_INDIVIDUAL_TARGETS :: [String]
        , bconfig_TARGETS :: [Target]
        , bconfig_TEST_QUICK_MODE :: Bool -- XXX This can be removed
        , bconfig_GHC_VERSION :: String
        , bconfig_BUILD_DIR :: String
        , bconfig_CABAL_BUILD_OPTIONS :: String
        , bconfig_CABAL_WITH_COMPILER :: String
        , bconfig_CABAL_EXECUTABLE :: String
        , bconfig_RTS_OPTIONS :: String
        , bconfig_TARGET_EXE_ARGS :: String -- XXX Can be removed, used in tests
        , bconfig_QUICK_MODE :: Bool
        , bconfig_SLOW :: Bool
        , bconfig_LONG :: Bool
        , bconfig_BENCH_PREFIX :: String
        , bconfig_GAUGE_ARGS :: String
        , bconfig_BENCHMARK_PACKAGE_VERSION :: String
        , bconfig_APPEND :: Bool
        , bconfig_COMMIT_COMPARE :: Bool
        , bconfig_BENCHMARK_PACKAGE_NAME :: String
        , bconfig_FIELDS :: [String]
        , bconfig_BENCH_CUTOFF_PERCENT :: Double
        , bconfig_BENCH_DIFF_STYLE :: GroupStyle
        , bconfig_SORT_BY_NAME :: Bool
        , bconfig_GRAPH :: Bool
        , bconfig_SILENT :: Bool
        , bconfig_RAW :: Bool
        , bconfig_MEASURE :: Bool
        , bconfig_ALL_FIELDS :: [String]
        , bconfig_INFINITE_GRP :: [Target]
        , bconfig_COMPARE :: Bool
        , bconfig_BENCH_SPEED_OPTIONS :: String -> String -> Maybe Quickness
        , bconfig_BENCH_RTS_OPTIONS :: String -> String -> String
        }

instance HasConfig Configuration where
    config_GROUP_TARGETS = bconfig_GROUP_TARGETS
    config_COMPARISONS = bconfig_COMPARISONS
    config_INDIVIDUAL_TARGETS = bconfig_INDIVIDUAL_TARGETS
    config_TARGETS = bconfig_TARGETS
    config_GHC_VERSION = bconfig_GHC_VERSION
    config_BUILD_DIR = bconfig_BUILD_DIR
    config_CABAL_BUILD_OPTIONS = bconfig_CABAL_BUILD_OPTIONS
    config_CABAL_WITH_COMPILER = bconfig_CABAL_WITH_COMPILER
    config_TEST_QUICK_MODE = bconfig_TEST_QUICK_MODE
    config_SILENT = bconfig_SILENT

defaultConfig :: Configuration
defaultConfig =
    Configuration
        { bconfig_RUNNING_DEVBUILD = False
        , bconfig_GROUP_TARGETS = Map.empty
        , bconfig_COMPARISONS = Map.empty
        , bconfig_INDIVIDUAL_TARGETS = []
        , bconfig_TARGETS = []
        , bconfig_CABAL_EXECUTABLE = "cabal"
        , bconfig_CABAL_WITH_COMPILER = "ghc"
        , bconfig_GHC_VERSION = ""
        , bconfig_CABAL_BUILD_OPTIONS = ""
        , bconfig_BUILD_DIR = "dist-newstyle"
        , bconfig_RTS_OPTIONS = ""

        -- Test specific
        , bconfig_TEST_QUICK_MODE = False
        , bconfig_TARGET_EXE_ARGS = ""

        -- Benchmark specific
        , bconfig_QUICK_MODE = False
        , bconfig_SLOW = False
        , bconfig_LONG = False
        , bconfig_BENCH_PREFIX = ""
        , bconfig_GAUGE_ARGS = ""
        , bconfig_BENCHMARK_PACKAGE_VERSION = "0.0.0"
        , bconfig_APPEND = False
        , bconfig_COMMIT_COMPARE = False
        , bconfig_BENCHMARK_PACKAGE_NAME = "streamly-benchmarks"
        -- XXX This is same as ALL_FIELDS
        , bconfig_FIELDS = ["cputime", "allocated", "maxrss"]
        , bconfig_BENCH_CUTOFF_PERCENT = 0
        , bconfig_BENCH_DIFF_STYLE = PercentDiff
        , bconfig_SORT_BY_NAME = False
        , bconfig_GRAPH = False
        , bconfig_SILENT = False
        , bconfig_RAW = False
        , bconfig_MEASURE = True
        , bconfig_ALL_FIELDS = ["cputime", "allocated", "maxrss"]
        , bconfig_INFINITE_GRP =
              [ TGroup "infinite_grp"
              ]
        , bconfig_COMPARE = False
        , bconfig_BENCH_SPEED_OPTIONS = \_ _ -> Nothing
        , bconfig_BENCH_RTS_OPTIONS = \_ _ -> ""
        }

type Context a = ReaderT Configuration IO a

--------------------------------------------------------------------------------
-- CLI
--------------------------------------------------------------------------------

targetsFromString :: String -> Either String [Target]
targetsFromString xs = Right $ map stringToTarget $ words xs

cliOptions :: OptParse.Parser Configuration
cliOptions = do
    Configuration <$> pure False -- switch (long "dev-build")
        <*> pure (bconfig_GROUP_TARGETS defaultConfig)
        <*> pure (bconfig_COMPARISONS defaultConfig)
        <*> pure (bconfig_INDIVIDUAL_TARGETS defaultConfig)
        <*> option
                 (eitherReader targetsFromString)
                 (long "targets"
                      <> value (bconfig_TARGETS defaultConfig))
        <*> pure (bconfig_TEST_QUICK_MODE defaultConfig)
        <*> pure (bconfig_GHC_VERSION defaultConfig)
        <*> pure (bconfig_BUILD_DIR defaultConfig)
        <*> strOption
              (long "cabal-build-options"
                   <> value (bconfig_CABAL_BUILD_OPTIONS defaultConfig))
        <*> strOption
              (long "with-compiler"
                   <> value (bconfig_CABAL_WITH_COMPILER defaultConfig))
        <*> pure (bconfig_CABAL_EXECUTABLE defaultConfig)
        <*> strOption
              (long "rts-opts" <> value (bconfig_RTS_OPTIONS defaultConfig))
        <*> pure (bconfig_TARGET_EXE_ARGS defaultConfig)
        <*> switch (long "quick")
        <*> switch (long "slow")
        <*> switch (long "long")
        <*> strOption
              (long "prefix" <> value (bconfig_BENCH_PREFIX defaultConfig))
        <*> strOption
              (long "gauge-args" <> value (bconfig_GAUGE_ARGS defaultConfig))
        <*> pure (bconfig_BENCHMARK_PACKAGE_VERSION defaultConfig)
        <*> switch (long "append")
        <*> switch (long "commit-compare")
        <*> pure (bconfig_BENCHMARK_PACKAGE_NAME defaultConfig)
        <*> (words
                 <$> strOption
                       (long "fields"
                            <> value (unwords (bconfig_FIELDS defaultConfig))))
        <*> (read
                 <$> strOption
                       (long "diff-cutoff-percent"
                            <> value
                                  (show
                                       (bconfig_BENCH_CUTOFF_PERCENT
                                            defaultConfig))))
        <*> option
              (eitherReader diffStyleFromString)
              (long "diff-style"
                   <> value (bconfig_BENCH_DIFF_STYLE defaultConfig))
        <*> switch (long "sort-by-name")
        <*> switch (long "graphs")
        <*> switch (long "silent")
        <*> switch (long "raw")
        <*> unswitch (long "no-measure")
        <*> pure (bconfig_ALL_FIELDS defaultConfig)
        <*> pure (bconfig_INFINITE_GRP defaultConfig)
        <*> switch (long "compare")
        <*> pure (bconfig_BENCH_SPEED_OPTIONS defaultConfig)
        <*> pure (bconfig_BENCH_RTS_OPTIONS defaultConfig)

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
    ---------------------------------------------------------------------------
    -- Determine the options
    ---------------------------------------------------------------------------

    benchSpeedOptions <- asks bconfig_BENCH_SPEED_OPTIONS
    benchRTSOptions <- asks bconfig_BENCH_RTS_OPTIONS
    let benchBaseName = takeFileName benchExecPath
    -- Using no name conversion
    let superQuickOptions = "--stdev 1000000"
        quickerOptions = "--stdev 100"
        localRTSOptions = benchRTSOptions benchBaseName benchName
    quickMode <- asks bconfig_QUICK_MODE
    globalRTSOptions <- asks bconfig_RTS_OPTIONS
    let rtsOptions1 = [str|+RTS -T #{localRTSOptions} #{globalRTSOptions} -RTS|]
    let quickBenchOptions =
            if quickMode
            then superQuickOptions
            else case benchSpeedOptions benchBaseName benchName of
                     Nothing -> ""
                     Just Quicker -> quickerOptions
                     Just SuperQuick -> superQuickOptions
    -- Skipping STREAM_LEN as it is just a display trick
    long_ <- asks bconfig_LONG
    let streamSize =
            if long_
            then Just (10000000 :: Int)
            else Nothing
    streamLen <-
        case show <$> streamSize of
            Just size ->
                liftIO
                    $ toLastLine
                          [str|env LC_ALL=en_US.UTF-8 printf #
                                 "--stream-size %'.f\n" #{size}|]
            Nothing -> return ""
    let streamSizeOpt =
            case show <$> streamSize of
                Just size -> [str|--stream-size #{size}|]
                Nothing -> ""
    -- benchName might have some single quotes and double quotes. We need to
    -- escape them.
    liftIO $ putStrLn $ compactWordsQuoted $ escapeAll
        [str| #{benchName}
                  #{rtsOptions1}
                  #{streamLen}
                  #{quickBenchOptions}
                  #{otherOptions}
        |]

    ----------------------------------------------------------------------------
    -- Run benchmark with options and collect results
    ----------------------------------------------------------------------------

    let outputFile = benchOutputFile benchBaseName
        outputDir = takeDirectory outputFile
    liftIO $ createDirectoryIfMissing True outputDir
    liftIO $ toStdout [str|rm -f #{outputFile}.tmp|]
    -- This is used inside two levels of quotes so escape twice
    let benchNameEscaped = shellEscape $ shellEscape benchName
    -- liftIO $ putStrLn $ "benchNameEscaped: " ++ benchNameEscaped
    let cmd = compactWordsQuoted [str|
                #{benchExecPath}
                    -j 1
                    #{rtsOptions1}
                    #{streamSizeOpt}
                    #{quickBenchOptions}
                    #{otherOptions}
                    --csv=#{outputFile}.tmp
                    -p '$0 == "'"#{benchNameEscaped}"'"'
              |]
    -- liftIO $ putStrLn $ "Running: " ++ cmd
    liftIO $ cmd `onError` die "Benchmark execution failed."

    -- Post-process the output
    -- Convert cpuTime field from picoseconds to seconds
    liftIO
        $ [str|awk --version 2>&1 | grep -q "GNU Awk" #
          |] `onError` die "Need GNU awk. [$(which awk)] is not GNU awk."
    liftIO
        $ toStdout [str|                                                 #
                tail -n +2 #{outputFile}.tmp                             #
                    | awk 'BEGIN {FPAT = "([^,]+)|(\"[^\"]+\")";OFS=","} #
                            {$2=$2/1000000000000;print}'                 #
                    >> #{outputFile}                                     #
              |]

invokeTastyBench :: String -> String -> String -> Context ()
invokeTastyBench targetProg targetName outputFile = do
    long_ <- asks bconfig_LONG
    benchPrefix <- asks bconfig_BENCH_PREFIX
    gaugeArgs <- asks bconfig_GAUGE_ARGS
    escapedBenchPrefix <-
        liftIO $ toLastLine [str|echo "#{benchPrefix}" | sed -e 's/\//\\\//g'|]
    let match
          | long_ = [str|-p "/#{targetName}\\/o-1-space/"|]
          | null benchPrefix = ""
          | otherwise = [str|-p "/#{escapedBenchPrefix}/"|]
    liftIO
        $ toStdoutV $ compactWordsQuoted
              [str| echo
                "Name,cpuTime,2*Stdev (ps),Allocated,bytesCopied,maxrss"
                >> #{outputFile}
             |]
    let cmd = [str|#{targetProg} -l #{match} | grep "^All"|]
    -- liftIO $ putStrLn $ "Command is: " ++ cmd
    benchmarkNames <- liftIO $ toLines cmd & Stream.toList
    for_ benchmarkNames $ \name -> benchExecOne targetProg name gaugeArgs

runBenchTarget :: String -> String -> String -> Context ()
runBenchTarget packageName component targetName = do
    benchmarkPackageVersion <- asks bconfig_BENCHMARK_PACKAGE_VERSION
    mTargetProg <-
        cabalTargetProg
            [str|#{packageName}-#{benchmarkPackageVersion}|]
            component
            targetName
    case mTargetProg of
        Nothing ->
            liftIO
                $ die [str|Cannot find executable for target #{targetName}|]
        Just targetProg -> do
            liftIO $ putStrLn [str|"Running executable #{targetName} ..."|]
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
    append <- asks bconfig_APPEND
    exists <- liftIO $ Test.test outputFile Test.exists
    when (not append && exists)
        $ liftIO $ toStdout [str|mv -f -v #{outputFile} #{outputFile}.prev|]

getBuildCommand :: Context String
getBuildCommand = do
    cabalExecutable <- asks bconfig_CABAL_EXECUTABLE
    withCompiler <- asks config_CABAL_WITH_COMPILER
    opts <- asks config_CABAL_BUILD_OPTIONS
    return $ compactWordsQuoted [str|
                #{cabalExecutable}
                    v2-build
                    --flag fusion-plugin
                    --flag limit-build-mem
                    --with-compiler #{withCompiler}
                    #{opts}
                    --enable-benchmarks
           |]

runMeasurements :: [String] -> Context ()
runMeasurements targets = do
    for_ targets backupOutputFile
    commitCompare <- asks bconfig_COMMIT_COMPARE
    buildBench <- getBuildCommand
    benchPackageName <- asks bconfig_BENCHMARK_PACKAGE_NAME
    if commitCompare
    then runBenchesComparing targets
    else do
        liftIO $ runBuild buildBench benchPackageName "bench" targets
        -- XXX What is target_exe_extra_args here?
        runBenchTargets benchPackageName "b" targets

runReports :: [String] -> Context ()
runReports benchmarks = do
    silent <- asks bconfig_SILENT
    graphs <- asks bconfig_GRAPH
    sortByName <- asks bconfig_SORT_BY_NAME
    diffStyle <- asks bconfig_BENCH_DIFF_STYLE
    cutOffPercent <- asks bconfig_BENCH_CUTOFF_PERCENT
    fields <- asks bconfig_FIELDS
    for_ benchmarks
        $ \i -> liftIO $ do
              unless silent $ putStrLn [str|Generating reports for #{i}...|]
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

printHelpOnArgs :: Context Bool
printHelpOnArgs = do
    _ <- printHelpOnTargets
    targets <- getTargets
    when (hasItem (TIndividual "help") targets) $ do
        listComparisons
    fields <- asks bconfig_FIELDS
    allFields <- asks bconfig_ALL_FIELDS
    when (hasItem "help" fields) $ liftIO $ do
        putStr "Supported fields: "
        putStrLn $ unwords allFields
        putStr "Default fields: "
        putStrLn $ unwords $ bconfig_FIELDS defaultConfig
    return $ hasItem "help" fields || hasItem (TIndividual "help") targets

-------------------------------------------------------------------------------
-- Determine targets
-------------------------------------------------------------------------------

flagLongSetup :: Configuration -> IO [Target]
flagLongSetup Configuration{..} = do
    let targetsStr = unwords $ catIndividuals bconfig_TARGETS
    when (bconfig_LONG && not (null bconfig_TARGETS))
        $ liftIO
        $ die [str|Cannot specify benchmarks [#{targetsStr}] with --long|]

    return
        $ if bconfig_LONG
          then bconfig_INFINITE_GRP
          else bconfig_TARGETS

-------------------------------------------------------------------------------
-- Build and run targets
-------------------------------------------------------------------------------

buildAndRunTargets :: Context ()
buildAndRunTargets = do
    measure <- asks bconfig_MEASURE
    targets <- getTargets
    when measure $ runMeasurements (catIndividuals targets)

-------------------------------------------------------------------------------
-- Run reports
-------------------------------------------------------------------------------

buildComparisonResults :: String -> [String] -> Context ()
buildComparisonResults name constituents = do
    liftIO $ createDirectoryIfMissing True [str|charts/#{name}|]
    let destFile = [str|charts/#{name}/results.csv|]
    liftIO $ toStdoutV $ compactWordsQuoted [str| : > #{destFile}|]
    for_ constituents
        $ \j ->
              liftIO
                  $ toStdoutV $ compactWordsQuoted
                        [str| cat "charts/#{j}/results.csv" >> #{destFile}|]

runFinalReports :: Context ()
runFinalReports = do
    compare <- asks bconfig_COMPARE
    targets <- getTargets
    comparisons <- asks bconfig_COMPARISONS
    raw <- asks bconfig_RAW
    let targetsStr = unwords $ catIndividuals targets
        individualTargets = catIndividuals targets
        comparisonTargets = catComparisons targets
    unless raw $ runReports individualTargets
    for_ comparisonTargets
        $ \i -> buildComparisonResults i (comparisons Map.! i)
    unless raw $ runReports comparisonTargets
    when compare
        $ do
            dynCmpGrpName <-
                liftIO
                    $ (++ "_cmp")
                    <$> toLastLine
                            [str|echo "#{targetsStr}" | sed -e 's/ /_/g'|]
            buildComparisonResults dynCmpGrpName individualTargets
            unless raw $ runReports [dynCmpGrpName]
            liftIO $ toStdoutV $ compactWordsQuoted
                       [str| rm -rf "charts/#{dynCmpGrpName}"|]

--------------------------------------------------------------------------------
-- Pipeline
--------------------------------------------------------------------------------

runPipeline :: Context ()
runPipeline = do
    hasHelp <- printHelpOnArgs
    unless hasHelp $ do
         printTargets
         buildAndRunTargets
         runFinalReports

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

mainWith ::
       [(String, [String])]
    -> (String -> String -> Maybe Quickness)
    -> (String -> String -> String)
    -> IO ()

-- XXX Use a defaultConfig record instead
mainWith targetMap speedOpts rtsOpts = do
    (conf, ()) <-
        simpleOptions
            "0.0.0"
            "bench"
            "A helper tool for benchmarking"
            cliOptions
            empty
    (cabalExe, buildDir) <- getCabalExe
    ghcVer <- getGhcVersion $ config_CABAL_WITH_COMPILER conf
    targets <- flagLongSetup conf
    let benchTargetMap = filter (\(_, xs) -> "noBench" `notElem` xs) targetMap
    let grpTargets = getGroupTargets "_grp" benchTargetMap
    let indTargets = map fst benchTargetMap
    let cmps = getGroupTargets "_cmp" benchTargetMap
    let conf1 =
            conf
              { bconfig_TARGETS = targets
              , bconfig_GROUP_TARGETS = grpTargets
              , bconfig_INDIVIDUAL_TARGETS = indTargets
              , bconfig_COMPARISONS = cmps
              , bconfig_BENCH_SPEED_OPTIONS = speedOpts
              , bconfig_BENCH_RTS_OPTIONS = rtsOpts
              , bconfig_BUILD_DIR = buildDir
              , bconfig_CABAL_EXECUTABLE = cabalExe
              , bconfig_GHC_VERSION = ghcVer
              }
    void $ runReaderT runPipeline conf1
