{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module BuildLib
    ( Configuration(..)
    , Context
    , hasItem
    , listTargets
    , listTargetGroups
    , listComparisons
    , setTargets
    , cabalTargetProg
    , setCommonVars
    , setDerivedVars
    , runBuild
    , defaultConfig
    , Quickness(..)
    , Target(..)
    , catIndividuals
    , catComparisons
    , stringToTarget
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import BenchShow.Internal.Common (GroupStyle(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.State.Strict (StateT, get, gets, put)
import Data.List (nub, sort, intercalate, isSuffixOf)
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Streamly.Coreutils.Which (which)
import Utils.QuasiQuoter (line)

import qualified Data.Map as Map
import qualified Streamly.Coreutils.FileTest as Test

import Utils

--------------------------------------------------------------------------------
-- Context
--------------------------------------------------------------------------------

data Target
    = TGroup String
    | TIndividual String
    | TComparisonGroup String
    deriving (Eq)

targetToString :: Target -> String
targetToString (TIndividual x) = x
targetToString (TGroup x) = x
targetToString (TComparisonGroup x) = x

stringToTarget :: String -> Target
stringToTarget x
    | "_cmp" `isSuffixOf` x = TComparisonGroup x
    | "_grp" `isSuffixOf` x = TGroup x
    | otherwise = TIndividual x

catIndividuals :: [Target] -> [String]
catIndividuals = mapMaybe f

    where

    f (TIndividual x) = Just x
    f _ = Nothing

catComparisons :: [Target] -> [String]
catComparisons = mapMaybe f

    where

    f (TComparisonGroup x) = Just x
    f _ = Nothing

data Quickness
    = Quicker
    | SuperQuick

data Configuration =
    Configuration
        { config_RUNNING_DEVBUILD :: Bool
        , config_GROUP_TARGETS :: Map String [String]
        , config_COMPARISONS :: Map String [String]
        , config_INDIVIDUAL_TARGETS :: [String]
        , config_TARGETS :: [Target]
        , config_TEST_QUICK_MODE :: Bool -- XXX This can be removed
        , config_GHC_VERSION :: String
        , config_BUILD_DIR :: String
        , config_CABAL_BUILD_OPTIONS :: String
        , config_CABAL_WITH_COMPILER :: String
        , config_CABAL_EXECUTABLE :: String
        , config_RTS_OPTIONS :: String
        , config_TARGET_EXE_ARGS :: String -- XXX Can be removed, used in tests
        , config_QUICK_MODE :: Bool
        , config_SLOW :: Bool
        , config_USE_GIT_CABAL :: Bool
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
        , config_ALL_FIELDS :: [String]
        , config_BUILD_FLAGS :: String -- XXX Can be removed
        , config_INFINITE_GRP :: [Target]
        , config_COMPARE :: Bool
        , config_BENCH_SPEED_OPTIONS :: String -> String -> Maybe Quickness
        , config_BENCH_RTS_OPTIONS :: String -> String -> String
        }

defaultConfig :: Configuration
defaultConfig =
    Configuration
        { config_RUNNING_DEVBUILD = False
        , config_GROUP_TARGETS = Map.empty
        , config_COMPARISONS = Map.empty
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
        , config_LONG = False
        , config_BENCH_PREFIX = ""
        , config_GAUGE_ARGS = ""
        , config_BENCHMARK_PACKAGE_VERSION = "0.0.0"
        , config_APPEND = False
        , config_COMMIT_COMPARE = False
        , config_BUILD_BENCH = ""
        , config_BENCHMARK_PACKAGE_NAME = "streamly-benchmarks"
        , config_FIELDS = ["cputime", "allocated", "maxrss"]
        , config_BENCH_CUTOFF_PERCENT = 0
        , config_BENCH_DIFF_STYLE = PercentDiff
        , config_SORT_BY_NAME = False
        , config_GRAPH = False
        , config_SILENT = False
        , config_RAW = False
        , config_MEASURE = True
        , config_DEFAULT_FIELDS = ["cputime", "allocated", "maxrss"]
        , config_ALL_FIELDS = ["allocated", "cputime", "allocated", "maxrss"]
        , config_BUILD_FLAGS = ""
        , config_INFINITE_GRP =
              [ TGroup "prelude_serial_grp"
              , TGroup "prelude_concurrent_grp"
              , TIndividual "Prelude.Rate"
              ]
        , config_COMPARE = False
        , config_BENCH_SPEED_OPTIONS = \_ _ -> Nothing
        , config_BENCH_RTS_OPTIONS = \_ _ -> ""
        }

-- Clean this, use both ReaderT and StateT!
type Context a = StateT Configuration IO a

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

hasItem :: Eq a => a -> [a] -> Bool
hasItem = elem

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
    let xs = Map.foldrWithKey (\k v b -> b ++ [pretty k v]) [] grpTargets
    liftIO $ putStrLn "Benchmark groups:"
    liftIO $ putStr $ unlines xs

    where

    pretty k v = k ++ " [" ++ intercalate ", " v ++ "]"

listComparisons :: Context ()
listComparisons = do
    liftIO $ putStrLn "Comparison groups:"
    res <- gets config_COMPARISONS
    let xs = Map.foldrWithKey (\k_ v_ b -> b ++ [pretty k_ v_]) [] res
    liftIO $ putStr $ unlines xs

    where

    pretty k v = k ++ " [" ++ intercalate ", " v ++ "]"

setTargets :: Context [Target]
setTargets = do
    conf <- get
    allIndividualTargets <- allGrp
    let defTargets = map TIndividual allIndividualTargets
        targets = config_TARGETS conf
        grpTargets = config_GROUP_TARGETS conf
        comparisons = config_COMPARISONS conf
    let newTargets =
            if null targets
            then defTargets
            else flatten grpTargets comparisons targets
    return newTargets

    where

    -- XXX Avoid explicit recursion
    flatten _ _ [] = []
    flatten grpTargets comparisons (t:ts) =
        t
            : case t of
                  TGroup x ->
                      map TIndividual ((Map.!) grpTargets x)
                          ++ flatten grpTargets comparisons ts
                  TComparisonGroup x ->
                      map TIndividual ((Map.!) comparisons x)
                          ++ flatten grpTargets comparisons ts
                  TIndividual _ -> flatten grpTargets comparisons ts

cabalWhichBuilddir :: String -> String -> String -> String -> Context String
cabalWhichBuilddir  builddir packageNameWithVersion component cmdToFind = do
    env_TEST_QUICK_MODE <- gets config_TEST_QUICK_MODE
    env_GHC_VERSION <- gets config_GHC_VERSION
    let noopt =
            if env_TEST_QUICK_MODE
            then "/noopt"
            else ""
        ghc = [line| $builddir/build/*/ghc-${env_GHC_VERSION} |]
        co = [line| $component/$cmdToFind$noopt/build/$cmdToFind/$cmdToFind |]
        path = [line| $ghc/$packageNameWithVersion/$co |]
    truePath <- liftIO $ runUtf8' [line| echo $path |]
    liftIO $ run [line| echo [cabal_which "$truePath"] 1>&2 |]
    liftIO $ runUtf8' [line| test -f "$truePath" && echo $truePath |]

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
    cabalExe <-
            if useGitCabal
            then do
                r <- liftIO $ which "git-cabal"
                case r of
                    Just _ -> do
                        liftIO
                            $ putStrLn
                                "Using git-cabal for branch specific builds"
                        return "git-cabal"
                    Nothing -> return "cabal"
            else return "cabal"
    buildDir <-
        if useGitCabal
        then liftIO $ runUtf8' "git-cabal show-builddir"
        else return "dist-newstyle"
    put
        $ conf
              { config_TARGET_EXE_ARGS = ""
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
