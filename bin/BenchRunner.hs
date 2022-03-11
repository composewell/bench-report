{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import BuildLib

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO(..))
import Data.Map (Map)
import Utils.QuasiQuoter (line)
import Control.Monad.Trans.State.Strict (StateT, get, gets, put)
import Data.List (isSuffixOf, nub, sort, intersperse)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeFileName)

import qualified Streamly.Coreutils.FileTest as Test
import qualified Data.Map as Map

import Utils

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

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = putStrLn "Hello World!"
