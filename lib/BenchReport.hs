{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module BenchReport
    ( Options (..)
    , BenchType (..)
    , defaultOptions
    , runBenchReport
    , ghcDumpdirName
    )
where

import Control.Exception (catch, ErrorCall(..))
import Data.Char (toLower)
import Data.List (sortOn)
#ifdef NO_CHARTS
import System.IO (hPutStrLn, stderr)
#endif

#ifdef NO_CHARTS
import BenchShow.Internal.Common
import BenchShow.Internal.Report
#else
import BenchShow
#endif

------------------------------------------------------------------------------
-- Options
------------------------------------------------------------------------------

data BenchType
    = Compare String
    | Standard String
    | CoreSize String -- ^ Module name; input is core-size/<mod>.core-sizes.csv
    deriving Show

data Options = Options
    { genGraphs :: Bool
    , sortByName :: Bool
    , useGauge :: Bool
    , benchType :: Maybe BenchType
    , fields :: [String]
    , diffStyle :: GroupStyle
    , cutOffPercent :: Double
    } deriving Show

defaultOptions :: Options
defaultOptions = Options False False False Nothing ["time"] PercentDiff 0

ignoringErr :: IO () -> IO ()
ignoringErr a = catch a (\(ErrorCall err :: ErrorCall) ->
    putStrLn $ "Failed with error:\n" <> err <> "\nSkipping.")

------------------------------------------------------------------------------
-- Generic
------------------------------------------------------------------------------

makeGraphs :: String -> Config -> String -> IO ()
makeGraphs name cfg inputFile =
#ifdef NO_CHARTS
    hPutStrLn stderr "Please compile without no-charts flag"
#else
    ignoringErr $ graph inputFile name cfg
#endif

------------------------------------------------------------------------------
-- Arrays
------------------------------------------------------------------------------

showComparisons :: Options -> Config -> FilePath -> FilePath -> IO ()
showComparisons Options{..} cfg inp out =
    let cfg1 = cfg { classifyBenchmark = classifyComparison }
     in if genGraphs
        then
#ifdef NO_CHARTS
            hPutStrLn stderr "Please compile without no-charts flag"
#else
            ignoringErr $ graph inp "comparison"
                cfg1 { outputDir = Just out
                     , presentation = Groups Absolute
                     }
#endif
        else ignoringErr $ report inp Nothing cfg1

    where

    separator = if useGauge then '/' else '.'
    dropComponent sep = dropWhile (== sep) . dropWhile (/= sep)

    -- In case of tasty-bench the names could be like
    -- All.Data.Array.Prim.Pinned/o-1-space.generation.show
    -- All.Data.Array.Foreign/o-1-space.generation.show
    classifyComparison b =
        let b1 =
                if useGauge
                then b
                else dropComponent separator b --- drop "All." at the beginning
         in Just
            ( takeWhile (/= '/') b1
            , dropComponent '/' b1 -- for tasty-bench drop up to "/"
            )

------------------------------------------------------------------------------
-- text reports
------------------------------------------------------------------------------

selectBench
    :: Options
    -> (SortColumn -> Maybe GroupStyle -> Either String [(String, Double)])
    -> [String]
selectBench Options{..} f =
    -- Apply filterPred only if at least 2 columns exist
    let colVals =
            case f (ColumnIndex 1) (Just PercentDiff) of
                Left _ -> either error id $ f (ColumnIndex 0) (Just PercentDiff)
                Right xs -> filter (filterPred . snd) xs
    in reverse
           $ fst <$> sortFunc colVals

    where

    sortFunc = if sortByName then sortOn fst else sortOn snd

    filterPred x
        | cutOffPercent > 0 = x >= cutOffPercent
        | cutOffPercent < 0 = x <= cutOffPercent
        | otherwise = True

benchShow ::
       Options
    -> Config
    -> (Config -> String -> IO ())
    -> String
    -> FilePath
    -> IO ()
benchShow Options{..} cfg func inp out =
    if genGraphs
    then func cfg {outputDir = Just out} inp
    else ignoringErr $ report inp Nothing cfg

ghcDumpdirName :: String
ghcDumpdirName = "dumpdir"

runBenchReport :: Options -> IO ()
runBenchReport opts@Options{fields = fs, benchType = btype} =
            let cfg = defaultConfig
                    { presentation = Groups (diffStyle opts)
                    , selectBenchmarks = selectBench opts
                    , selectFields = filter
                        ( flip elem (fmap (fmap toLower) fs)
                        . fmap toLower
                        )
                    }
            in case btype of
                Just (Compare str) ->
                    showComparisons opts cfg
                        { title = Just str }
                        ("charts/" ++ str ++ "/results.csv")
                        ("charts/" ++ str)
                Just (Standard str) ->
                    benchShow opts cfg
                        { title = Just str }
                        (makeGraphs str)
                        ("charts/" ++ str ++ "/results.csv")
                        ("charts/" ++ str)
                Just (CoreSize str) ->
                    benchShow opts cfg
                        { title = Just str }
                        (makeGraphs str)
                        (ghcDumpdirName ++ "/" ++ str ++ ".core-sizes.csv")
                        ghcDumpdirName
                Nothing ->
                    error "Please specify a benchmark using --benchmark."
