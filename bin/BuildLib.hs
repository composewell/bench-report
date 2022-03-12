{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module BuildLib
    ( Configuration(..)
    , Context
    , die
    , warn
    , runVerbose
    , hasItem
    , silently
    , testOnly
    , benchOnly
    , devBuild
    , allGrp
    , allTargetGroups
    , listTargets
    , listTargetGroups
    , setTargets
    , cabalWhichBuilddir
    , cabalWhich
    , cabalTargetProg
    , setCommonVars
    , setDerivedVars
    , runBuild
    , defaultConfig
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO(..))
import Data.Map (Map)
import Utils.QuasiQuoter (line)
import Control.Monad.Trans.State.Strict (StateT, get, gets, put)
import Data.List (isSuffixOf, nub, sort, intersperse)
import BenchShow.Internal.Common (GroupStyle(..))

import qualified Streamly.Coreutils.FileTest as Test
import qualified Data.Map as Map

import Utils

--------------------------------------------------------------------------------
-- Context
--------------------------------------------------------------------------------

data Configuration =
    Configuration
        { config_RUNNING_TESTS :: Bool
        , config_RUNNING_BENCHMARKS :: Bool
        , config_RUNNING_DEVBUILD :: Bool
        , config_GROUP_TARGETS :: Map String [String]
        , config_COMPARISIONS :: Map String [String]
        , config_INDIVIDUAL_TARGETS :: [String]
        , config_TARGETS :: [String]
        , config_TEST_QUICK_MODE :: Bool
        , config_GHC_VERSION :: String
        , config_BUILD_DIR :: String
        , config_CABAL_BUILD_OPTIONS :: String
        , config_CABAL_WITH_COMPILER :: String
        , config_CABAL_EXECUTABLE :: String
        , config_RTS_OPTIONS :: String
        , config_TARGET_EXE_ARGS :: String
        , config_QUICK_MODE :: Bool
        , config_SLOW :: Bool
        , config_USE_GIT_CABAL :: Bool
        , config_DEFAULT_TARGETS :: [String]
        , config_LONG :: Bool
        , config_BENCH_PREFIX :: String
        , config_GAUGE_ARGS :: String
        , config_BENCHMARK_PACKAGE_VERSION :: String
        , config_APPEND :: Bool
        , config_COMMIT_COMPARE :: Bool
        , config_BUILD_BENCH :: String
        , config_BENCHMARK_PACKAGE_NAME :: String
        , config_FIELDS :: [String]
        , config_BENCH_CUTOFF_PERCENT :: Double
        , config_BENCH_DIFF_STYLE :: GroupStyle
        , config_SORT_BY_NAME :: Bool
        , config_GRAPH :: Bool
        , config_SILENT :: Bool
        , config_RAW :: Bool
        , config_MEASURE :: Bool
        , config_DEFAULT_FIELDS :: [String]
        , config_COMMON_FIELDS :: [String]
        , config_ALL_FIELDS :: [String]
        , config_TARGETS_ORIG :: [String]
        , config_BUILD_FLAGS :: String
        , config_SET_TARGETS :: [String]
        , config_ALL_GRP :: [String]
        , config_INFINITE_GRP :: [String]
        , config_COMPARISON_REPORTS :: [String]
        , config_COMPARE :: Bool
        }

defaultConfig :: Configuration
defaultConfig =
    Configuration
        { config_RUNNING_TESTS = False
        , config_RUNNING_BENCHMARKS = True
        , config_RUNNING_DEVBUILD = False
        , config_GROUP_TARGETS = Map.empty
        , config_COMPARISIONS = Map.empty
        , config_INDIVIDUAL_TARGETS = []
        , config_TARGETS = []
        , config_TEST_QUICK_MODE = False
        , config_GHC_VERSION = ""
        , config_BUILD_DIR = ""
        , config_CABAL_BUILD_OPTIONS = ""
        , config_CABAL_WITH_COMPILER = ""
        , config_CABAL_EXECUTABLE = ""
        , config_RTS_OPTIONS = ""
        , config_TARGET_EXE_ARGS = ""
        , config_QUICK_MODE = False
        , config_SLOW = False
        , config_USE_GIT_CABAL = True
        , config_DEFAULT_TARGETS = []
        , config_LONG = False
        , config_BENCH_PREFIX = ""
        , config_GAUGE_ARGS = ""
        , config_BENCHMARK_PACKAGE_VERSION = "0.0.0"
        , config_APPEND = False
        , config_COMMIT_COMPARE = False
        , config_BUILD_BENCH = ""
        , config_BENCHMARK_PACKAGE_NAME = "streamly-benchmarks"
        , config_FIELDS = []
        , config_BENCH_CUTOFF_PERCENT = 0
        , config_BENCH_DIFF_STYLE = PercentDiff
        , config_SORT_BY_NAME = False
        , config_GRAPH = False
        , config_SILENT = False
        , config_RAW = False
        , config_MEASURE = True
        , config_DEFAULT_FIELDS = []
        , config_COMMON_FIELDS = []
        , config_ALL_FIELDS = []
        , config_TARGETS_ORIG = []
        , config_BUILD_FLAGS = ""
        , config_SET_TARGETS = []
        , config_ALL_GRP = []
        , config_INFINITE_GRP = []
        , config_COMPARISON_REPORTS = []
        , config_COMPARE = False
        }

-- Clean this, use both ReaderT and StateT!
type Context a = StateT Configuration IO a

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

hasItem :: Eq a => a -> [a] -> Bool
hasItem = elem

testOnly :: String -> Context (Maybe String)
testOnly x = do
    res <- gets config_RUNNING_TESTS
    return
        $ if res
          then Just x
          else Nothing

benchOnly :: String -> Context (Maybe String)
benchOnly x = do
    res <- gets config_RUNNING_BENCHMARKS
    return
        $ if res
          then Just x
          else Nothing

devBuild :: String -> Context (Maybe String)
devBuild x = do
    res <- gets config_RUNNING_DEVBUILD
    return
        $ if res
          then Just x
          else Nothing

allGrp :: Context [String]
allGrp = do
    gtList <- Map.foldl' (++) [] <$> gets config_GROUP_TARGETS
    itList <- gets config_INDIVIDUAL_TARGETS
    return $ nub $ sort $ gtList ++ itList

allTargetGroups :: Context [String]
allTargetGroups = Map.keys <$> gets config_GROUP_TARGETS

listTargets :: Context ()
listTargets = do
    res <- allGrp
    liftIO $ putStrLn "Individual targets:"
    liftIO $ putStr $ unlines res

-- XXX pass as arg?
listTargetGroups :: Context ()
listTargetGroups = do
    grpTargets <- gets config_GROUP_TARGETS
    let xs = Map.foldrWithKey (\k_ v_ b -> b ++ [pretty k_ v_]) [] grpTargets
    liftIO $ putStr $ unlines xs

    where

    pretty k v = k ++ " [" ++ concat (intersperse ", " v) ++ "]"

-- XXX pass as arg?
setTargets :: Context [String]
setTargets = do
    conf <- get
    let defTargets = config_DEFAULT_TARGETS conf
        targets = config_TARGETS conf
        grpTargets = config_GROUP_TARGETS conf
        comparisions = config_COMPARISIONS conf
    let newTargets =
            if null targets
            then defTargets
            else flatten grpTargets comparisions targets
    return newTargets

    where

    flatten _ _ [] = []
    flatten grpTargets comparisions (t:ts)
        | "_grp" `isSuffixOf` t =
            (Map.!) grpTargets t ++ flatten grpTargets comparisions ts
        | "_cmp" `isSuffixOf` t =
            (Map.!) comparisions t ++ [t] ++ flatten grpTargets comparisions ts
        | otherwise = [t] ++ flatten grpTargets comparisions ts

cabalWhichBuilddir :: String -> String -> String -> String -> Context String
cabalWhichBuilddir  builddir packageNameWithVersion component cmdToFind = do
    env_TEST_QUICK_MODE <- gets config_TEST_QUICK_MODE
    env_GHC_VERSION <- gets config_GHC_VERSION
    let noopt =
            if env_TEST_QUICK_MODE
            then "/noopt"
            else ""
        path = [line|
$builddir/build/*/ghc-${env_GHC_VERSION}
/$packageNameWithVersion/$component
/$cmdToFind$noopt
/build/$cmdToFind/$cmdToFind
|]
    liftIO $ run [line| echo "[cabal_which $path]" 1>&2 |]
    liftIO $ runUtf8' [line| test -f "$path" && echo $path |]

cabalWhich :: String -> String -> String -> Context String
cabalWhich packageNameWithVersion component cmdToFind = do
    builddir <- gets config_BUILD_DIR
    cabalWhichBuilddir builddir packageNameWithVersion component cmdToFind

cabalTargetProg :: String -> String -> String -> Context (Maybe String)
cabalTargetProg packageNameWithVersion component target = do
    targetProg <- cabalWhich packageNameWithVersion component target
    -- XXX Check if executable
    res <- liftIO $ Test.test targetProg Test.exists
    if res
    then return (Just targetProg)
    else return Nothing

setCommonVars :: Context ()
setCommonVars = do
    conf <- get
    let useGitCabal = config_USE_GIT_CABAL conf
        cabalExe =
            if useGitCabal
            then "git-cabal"
            else "cabal"
    buildDir <-
        if useGitCabal
        then liftIO $ runUtf8' "git-cabal show-builddir"
        else return "dist-newstyle"
    put
        $ conf
              { config_SLOW = False
              , config_QUICK_MODE = False
              , config_RUNNING_DEVBUILD = False
              , config_TARGET_EXE_ARGS = ""
              , config_RTS_OPTIONS = ""
              , config_CABAL_BUILD_OPTIONS = ""
              , config_CABAL_EXECUTABLE = cabalExe
              , config_BUILD_DIR = buildDir
              }

setDerivedVars :: Context ()
setDerivedVars = do
    conf <- get
    let withCompiler = config_CABAL_WITH_COMPILER conf
    if null withCompiler
    then put $ conf {config_CABAL_WITH_COMPILER = "ghc"}
    else put
             $ conf
                   { config_CABAL_BUILD_OPTIONS =
                         let opts = config_CABAL_BUILD_OPTIONS conf
                          in [line| $opts --with-compiler $withCompiler |]
                   }
    conf1 <- get
    let withCompiler1 = config_CABAL_WITH_COMPILER conf1
    ghcVersion <- liftIO $ runUtf8' [line| $withCompiler1 --numeric-version |]
    put $ conf {config_GHC_VERSION = ghcVersion}

runBuild :: String -> String -> String -> [String] -> Context ()
runBuild buildProg package componentPrefix components = do
    let componentsWithContext =
            map (\c -> [line| $package:$componentPrefix:$c |]) components
        componentsWithContextStr = unwords componentsWithContext
    liftIO $ runVerbose [line| $buildProg $componentsWithContextStr |]
