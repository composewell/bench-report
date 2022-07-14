{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module BuildLib
    ( hasItem
    , HasConfig (..)
    , listTargets
    , listTargetGroups
    , listComparisons
    , getGroupTargets
    , getTargets
    , printTargets
    , printHelpOnTargets
    , cabalTargetProg
    , runBuild
    , Quickness(..)
    , Target(..)
    , catIndividuals
    , catComparisons
    , stringToTarget

    , getCabalExe
    , getGhcVersion
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (ReaderT, asks)
import Data.List (nub, sort, intercalate, isSuffixOf)
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Streamly.Coreutils.Which (which)
import Streamly.Internal.Unicode.String (str)

import qualified Data.List as List
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

getGroupTargets :: String -> [(String, [String])] -> Map String [String]
getGroupTargets suffix = List.foldl' f Map.empty

    where

    f b (k, xs) =
        let xs1 = filter (\x -> suffix `isSuffixOf` x) xs
         in List.foldl' (flip (Map.alter (updater k))) b xs1

    updater k Nothing = Just [k]
    -- XXX insert if not present instead of nub
    updater k (Just xs) = Just $ List.nub (k : xs)

{-
targetToString :: Target -> String
targetToString (TIndividual x) = x
targetToString (TGroup x) = x
targetToString (TComparisonGroup x) = x
-}

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

class HasConfig a where
        config_GROUP_TARGETS :: a -> Map String [String]
        config_COMPARISONS :: a -> Map String [String]
        config_INDIVIDUAL_TARGETS :: a -> [String]
        config_TARGETS :: a -> [Target]
        config_GHC_VERSION :: a -> String
        config_BUILD_DIR :: a -> String
        config_CABAL_BUILD_OPTIONS :: a -> String
        config_CABAL_WITH_COMPILER :: a -> String
        config_TEST_QUICK_MODE :: a -> Bool
        config_SILENT :: a -> Bool

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

hasItem :: Eq a => a -> [a] -> Bool
hasItem = elem

{-
devBuild :: HasConfig e => String -> ReaderT e IO (Maybe String)
devBuild x = do
    res <- gets config_RUNNING_DEVBUILD
    return
        $ if res
          then Just x
          else Nothing
              -}

allGrp :: HasConfig e => ReaderT e IO [String]
allGrp = do
    gtList <- Map.foldl' (++) [] <$> asks config_GROUP_TARGETS
    itList <- asks config_INDIVIDUAL_TARGETS
    return $ nub $ sort $ gtList ++ itList

{-
allTargetGroups :: HasConfig e => ReaderT e IO [String]
allTargetGroups = Map.keys <$> gets config_GROUP_TARGETS
-}

listTargets :: HasConfig e => ReaderT e IO ()
listTargets = do
    res <- allGrp
    liftIO $ putStrLn "Individual targets:"
    liftIO $ putStr $ unlines res

-- XXX pass as arg?
listTargetGroups :: HasConfig e => ReaderT e IO ()
listTargetGroups = do
    grpTargets <- asks config_GROUP_TARGETS
    let xs = Map.foldrWithKey (\k v b -> b ++ [pretty k v]) [] grpTargets
    liftIO $ putStrLn "Benchmark groups:"
    liftIO $ putStr $ unlines xs

    where

    pretty k v = k ++ " [" ++ intercalate ", " v ++ "]"

listComparisons :: HasConfig e => ReaderT e IO ()
listComparisons = do
    liftIO $ putStrLn "Comparison groups:"
    res <- asks config_COMPARISONS
    let xs = Map.foldrWithKey (\k_ v_ b -> b ++ [pretty k_ v_]) [] res
    liftIO $ putStr $ unlines xs

    where

    pretty k v = k ++ " [" ++ intercalate ", " v ++ "]"

getTargets :: HasConfig e => ReaderT e IO [Target]
getTargets = do
    targets <- asks config_TARGETS
    grpTargets <- asks config_GROUP_TARGETS
    comparisons <- asks config_COMPARISONS
    allIndividualTargets <- allGrp
    let defTargets = map TIndividual allIndividualTargets
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

printTargets :: HasConfig e => ReaderT e IO ()
printTargets = do
    silent <- asks config_SILENT
    targetsStr <- unwords . catIndividuals <$> getTargets
    unless silent
        $ liftIO $ putStrLn [str|"Using targets [#{targetsStr}]"|]

printHelpOnTargets :: HasConfig e => ReaderT e IO Bool
printHelpOnTargets = do
    targets <- getTargets
    if hasItem (TIndividual "help") targets
    then do
        listTargets
        listTargetGroups
        return True
    else return False

cabalWhichBuilddir :: HasConfig e =>
    String -> String -> String -> String -> ReaderT e IO String
cabalWhichBuilddir  builddir packageNameWithVersion component cmdToFind = do
    env_TEST_QUICK_MODE <- asks config_TEST_QUICK_MODE
    env_GHC_VERSION <- asks config_GHC_VERSION
    let noopt =
            if env_TEST_QUICK_MODE
            then "/noopt"
            else ""
        ghc = [str|#{builddir}/build/*/ghc-#{env_GHC_VERSION}|]
        co = [str|#{component}/#{cmdToFind}#{noopt}/build/#{cmdToFind}/#{cmdToFind}|]
        path = [str|#{ghc}/#{packageNameWithVersion}/#{co}|]
    truePath <- liftIO $ toLastLine [str|echo #{path}|]
    -- liftIO $ run [str|echo [cabal_which "$truePath"] 1>&2|]
    liftIO $ toLastLine [str|test -f "#{truePath}" && echo #{truePath}|]

cabalWhich :: HasConfig e => String -> String -> String -> ReaderT e IO String
cabalWhich packageNameWithVersion component cmdToFind = do
    builddir <- asks config_BUILD_DIR
    cabalWhichBuilddir builddir packageNameWithVersion component cmdToFind

cabalTargetProg :: HasConfig e =>
    String -> String -> String -> ReaderT e IO (Maybe String)
cabalTargetProg packageNameWithVersion component target = do
    targetProg <- cabalWhich packageNameWithVersion component target
    -- XXX Check if executable
    res <- liftIO $ Test.test targetProg Test.isExisting
    if res
    then return (Just targetProg)
    else return Nothing

getCabalExe :: IO (String, String)
getCabalExe = do
    r <- liftIO $ which "git-cabal"
    case r of
        Just _ -> do
            liftIO
                $ putStrLn
                    "Using git-cabal for branch specific builds"
            d <- toLastLine "git-cabal show-builddir"
            return ("git-cabal", d)
        Nothing -> return ("cabal", "dist-newstyle")

getGhcVersion :: String -> IO String
getGhcVersion ghc = liftIO $ toLastLine [str|#{ghc} --numeric-version|]

runBuild :: String -> String -> String -> [String] -> IO ()
runBuild buildProg package componentPrefix components = do
    let componentsWithContext =
            map (\c -> [str|#{package}:#{componentPrefix}:#{c}|]) components
        componentsWithContextStr = unwords componentsWithContext
    toStdoutV [str|#{buildProg} #{componentsWithContextStr}|]
