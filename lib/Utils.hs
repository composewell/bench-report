{-# LANGUAGE ScopedTypeVariables #-}

module Utils
    ( die
    , warn
    , run
    , runVerbose
    , runUtf8
    , runUtf8'
    , run_
    , silently
    , env_SCRIPT_DIR
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Catch (MonadCatch(..))
import Data.Function ((&))
import Data.Maybe (fromJust)
import System.FilePath (takeDirectory)
import System.Environment (getExecutablePath)
import Streamly.Internal.System.Process (ProcessFailure)

import qualified System.Exit as Exit (die)

import qualified Streamly.Internal.Console.Stdio as Stdio
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import qualified Streamly.Internal.Unicode.Stream as Unicode
import qualified Streamly.System.Process as Process
import qualified Streamly.System.Sh as Sh

import Utils.QuasiQuoter (line)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- XXX Re-evaluate the names of these helpers

die :: String -> IO ()
die x = Exit.die [line| Error: $x |]

warn :: String -> IO ()
warn x = Exit.die [line| Warning: $x |]

run :: String -> IO ()
run cmd = Sh.srcWith Process.toChunks cmd & Stdio.putChunks

runVerbose :: String -> IO ()
runVerbose cmd = putStrLn cmd >> run cmd

runUtf8 :: String -> Stream.SerialT IO String
runUtf8 cmd =
    Sh.srcWith Process.toChunks cmd & Unicode.decodeUtf8Arrays
        & Stream.splitOnSuffix (== '\n') Fold.toList

runUtf8' :: String -> IO String
runUtf8' cmd = fmap fromJust (runUtf8 cmd & Stream.last)

-- Run the command and return the exit status as a Bool. Note that this does not
-- throw any error or stop the execution. This is like a failable runner.
run_ :: String -> IO Bool
run_ cmd =
    putStrLn cmd
        >> catch
              (run cmd >> return True)
              (\(_ :: ProcessFailure) -> return False)

silently :: String -> IO ()
silently cmd = run [line| $cmd &> /dev/null |]

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

env_SCRIPT_DIR :: MonadIO m => m FilePath
env_SCRIPT_DIR = takeDirectory <$> liftIO getExecutablePath
