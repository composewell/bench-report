{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module TestRunner
    ( mainWith
    )
where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Monad (when, unless, void)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import System.FilePath (takeFileName, takeDirectory)
import System.Environment (setEnv)
import Streamly.Internal.Unicode.String (str)

import qualified Data.Map as Map
import qualified Options.Applicative as OptParse
import qualified Streamly.Coreutils.FileTest as FileTest

import Utils
import BuildLib

import Prelude hiding (compare)
import Options.Applicative hiding (Parser, str)
import Options.Applicative.Simple hiding (str)

-- XXX Abstract out config that is common to tests and benchmarks
data Configuration =
    Configuration
        { bconfig_RUNNING_DEVBUILD :: Bool
        , bconfig_GROUP_TARGETS :: Map String [String]
        , bconfig_COMPARISONS :: Map String [String]
        , bconfig_INDIVIDUAL_TARGETS :: [String]
        , bconfig_TARGETS :: [Target]
        , bconfig_TEST_QUICK_MODE :: Bool
        , bconfig_GHC_VERSION :: String
        , bconfig_BUILD_DIR :: String
        , bconfig_CABAL_BUILD_OPTIONS :: String
        , bconfig_CABAL_WITH_COMPILER :: String
        , bconfig_CABAL_EXECUTABLE :: String
        , bconfig_RTS_OPTIONS :: String
        , bconfig_BENCH_RTS_OPTIONS :: String -> String -> String
        , bconfig_SILENT :: Bool
        , bconfig_MEASURE :: Bool
        , bconfig_BENCHMARK_PACKAGE_NAME :: String
        , bconfig_BENCHMARK_PACKAGE_VERSION :: String
        , bconfig_GAUGE_ARGS :: String
        , bconfig_RAW :: Bool

        , bconfig_COVERAGE :: Bool
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
        -- Common to benchmarks and tests
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
        , bconfig_SILENT = False
        , bconfig_MEASURE = True
        , bconfig_BENCHMARK_PACKAGE_NAME = "streamly-tests"
        , bconfig_BENCHMARK_PACKAGE_VERSION = "0.0.0"
        , bconfig_GAUGE_ARGS = ""
        , bconfig_BENCH_RTS_OPTIONS = \_ _ -> ""
        , bconfig_RAW = False

        -- Test specific
        , bconfig_TEST_QUICK_MODE = False
        , bconfig_COVERAGE = False
        }

type Context a = ReaderT Configuration IO a

--------------------------------------------------------------------------------
-- CLI
--------------------------------------------------------------------------------

targetsFromString :: String -> Either String [Target]
targetsFromString xs = Right $ map stringToTarget $ words xs

cliOptions :: OptParse.Parser Configuration
cliOptions = do
    Configuration
        <$> switch (long "dev-build")
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
        <*> pure (bconfig_BENCH_RTS_OPTIONS defaultConfig)
        <*> switch (long "silent")
        <*> unswitch (long "no-measure")
        <*> pure (bconfig_BENCHMARK_PACKAGE_NAME defaultConfig)
        <*> pure (bconfig_BENCHMARK_PACKAGE_VERSION defaultConfig)
        <*> strOption
              (long "gauge-args" <> value (bconfig_GAUGE_ARGS defaultConfig))
        <*> switch (long "raw")
        <*> switch (long "coverage")

    where

    unswitch = flag True False

--------------------------------------------------------------------------------
-- Determine options from benchmark name
--------------------------------------------------------------------------------

-- XXX Make this common to becnhmarks and tests
getRTSOptions benchExecPath benchName = do
    benchRTSOptions <- asks bconfig_BENCH_RTS_OPTIONS
    let benchBaseName = takeFileName benchExecPath
        localRTSOptions = benchRTSOptions benchBaseName benchName
    globalRTSOptions <- asks bconfig_RTS_OPTIONS
    return [str|+RTS -T #{localRTSOptions} #{globalRTSOptions} -RTS|]

invokeHspec :: String -> String -> Context ()
invokeHspec targetProg targetName = do
    rtsOpts <- getRTSOptions targetProg targetName
    otherOptions <- asks bconfig_GAUGE_ARGS
    let cmd = [str|#{targetProg} #{rtsOpts} #{otherOptions}|]
    liftIO $ putStrLn $ compactWordsQuoted cmd
    liftIO $ cmd `onError` die "Target execution failed."

getSystem :: IO String
getSystem = do
    uname <- toLastLine "uname"
    if uname == "Linux"
    then return "x86_64-linux"
    else error $ "Unsupported system: " ++ uname

streamlySrcDir = "."
streamlyVer = "0.8.2"
streamlyPkg = [str|streamly-#{streamlyVer}|]

streamlyCoreSrcDir = "core"
streamlyCoreVer = "0.1.0"
streamlyCorePkg = [str|streamly-core-#{streamlyCoreVer}|]

getHPCPrefix pkg = do
    buildDir <- asks bconfig_BUILD_DIR
    ghcVer <- asks bconfig_GHC_VERSION
    system <- liftIO getSystem
    return [str|#{buildDir}/build/#{system}/ghc-#{ghcVer}/#{pkg}/hpc/vanilla|]

getTixFile target = do
    prefix <- getHPCPrefix streamlyPkg
    -- liftIO $ putStrLn $ "[" ++ prefix ++ "]"
    return [str|#{prefix}/tix/#{target}/#{target}.tix|]

getMixDirs = mapM f [streamlyPkg, streamlyCorePkg]

    where

    f pkg = do
        prefix <- getHPCPrefix pkg
        return [str|#{prefix}/mix/#{pkg}|]

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
            coverage <- asks bconfig_COVERAGE
            if coverage
            then do
                tixFile <- getTixFile targetName
                let tixDir = takeDirectory tixFile
                liftIO $ toStdout [str|mkdir -p #{tixDir}|]
                liftIO $ setEnv "HPCTIXFILE" tixFile
                invokeHspec targetProg targetName
                liftIO $ toStdout [str|rmdir #{tixDir} 2>/dev/null || true|]
            else invokeHspec targetProg targetName

runBenchTargets :: String -> String -> [String] -> Context ()
runBenchTargets packageName component targets =
    for_ targets $ runBenchTarget packageName component

getBuildCommand :: Context String
getBuildCommand = do
    cabalExecutable <- asks bconfig_CABAL_EXECUTABLE
    withCompiler <- asks config_CABAL_WITH_COMPILER
    opts <- asks config_CABAL_BUILD_OPTIONS

    devBuild <- asks bconfig_RUNNING_DEVBUILD
    let dev = if devBuild then " --flag dev " else ""
    quickMode <- asks bconfig_TEST_QUICK_MODE
    let quick =
            if quickMode then " --disable-optimization --flags -opt " else ""
    coverage <- asks bconfig_COVERAGE
    let projectFile =
            if coverage then " --project-file cabal.project.coverage " else ""

    -- With the --enable-coverage option the tests as well as the library get
    -- compiled with -fhpc, and we get coverage for tests as well. But we want
    -- to exclude that, so a project file is needed.
    return $ compactWordsQuoted [str|
                #{cabalExecutable}
                    v2-build
                    #{projectFile}
                    --flag limit-build-mem
                    #{dev}
                    #{quick}
                    --with-compiler #{withCompiler}
                    #{opts}
                    --enable-tests
           |]

runMeasurements :: [String] -> Context ()
runMeasurements targets = do
    buildCmd <- getBuildCommand
    benchPackageName <- asks bconfig_BENCHMARK_PACKAGE_NAME
    buildableTargets <-
        liftIO $ runBuild buildCmd benchPackageName "test" targets

    coverage <- asks bconfig_COVERAGE
    when coverage $ do
        buildDir <- asks bconfig_BUILD_DIR
        liftIO $ toStdout [str|mkdir -p #{buildDir}/hpc|]
    runBenchTargets benchPackageName "t" buildableTargets

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

runFinalReports :: Context ()
runFinalReports = do
    raw <- asks bconfig_RAW
    coverage <- asks bconfig_COVERAGE
    let hpcOptions = ""
    -- hpcOptions <- asks bconfig_HPC_OPTIONS

    when (not raw && coverage) $ do
        targets <- getTargets
        xs <- mapM getTixFile (catIndividuals targets)
        -- Some tix files are genuinely not generated
        -- Remove non-existing files otherwise hpc fails
        xs1 <- liftIO $ mapM checkFile xs
        let tixFiles = unwords $ catMaybes xs1
        buildDir <- asks bconfig_BUILD_DIR
        let allTix = [str|#{buildDir}/hpc/all.tix|]
        -- XXX allTix/tixFiles cannot have double quotes in it
        liftIO
            $ toStdoutV [str|hpc sum --union --output="#{allTix}" #{tixFiles}|]

        let srcDirs =
                unwords
                    $ map
                        (\x -> [str|--srcdir "#{x}"|])
                        [streamlySrcDir, streamlyCoreSrcDir]
        mixDirs <- getMixDirs
        let mixArgs = unwords $ map (\x -> [str|--hpcdir "#{x}"|]) mixDirs
        liftIO $ toStdoutV [str|hpc markup "#{allTix}" #{mixArgs} #{srcDirs}|]
        liftIO
            $ toStdoutV
                [str|hpc report "#{allTix}" #{hpcOptions} #{mixArgs} #{srcDirs}|]
        return ()

        where

        checkFile fp = do
            r <- FileTest.test fp FileTest.isExisting
            if r
            then return $ Just fp
            else do
                putStrLn $ "WARNING! Ignoring non-existing tix file " ++ fp
                return Nothing

--------------------------------------------------------------------------------
-- Pipeline
--------------------------------------------------------------------------------

runPipeline :: Context ()
runPipeline = do
    hasHelp <- printHelpOnTargets
    unless hasHelp $ do
         printTargets
         buildAndRunTargets
         runFinalReports

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

mainWith ::
       [(String, [String])]
    -> (Bool -> String -> String -> String)
    -> IO ()
mainWith targetMap rtsOpts = do
    (conf, ()) <-
        simpleOptions
            "0.0.0"
            "testRunner"
            "A helper tool for running tests"
            cliOptions
            empty
    (cabalExe, buildDir) <- getCabalExe
    ghcVer <- getGhcVersion $ config_CABAL_WITH_COMPILER conf
    let moreMem = bconfig_COVERAGE conf || bconfig_RUNNING_DEVBUILD conf

    let testTargetMap = filter (\(_, xs) -> "noTest" `notElem` xs) targetMap
    let testTargetMap1 =
            if bconfig_RUNNING_DEVBUILD conf
            then targetMap
            else filter (\(_, xs) -> "testDevOnly" `notElem` xs) testTargetMap
    let grpTargets = getGroupTargets "_grp" testTargetMap1
    let indTargets = map fst testTargetMap1
    let conf1 =
            conf
              { bconfig_GROUP_TARGETS = grpTargets
              , bconfig_INDIVIDUAL_TARGETS = indTargets
              , bconfig_BUILD_DIR = buildDir
              , bconfig_CABAL_EXECUTABLE = cabalExe
              , bconfig_GHC_VERSION = ghcVer
              , bconfig_BENCH_RTS_OPTIONS = rtsOpts moreMem
              }
    void $ runReaderT runPipeline conf1
