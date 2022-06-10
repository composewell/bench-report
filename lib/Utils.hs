{-# LANGUAGE ScopedTypeVariables #-}

module Utils
    ( die
    , warn
    , Sh.toStdout
    , toStdoutV
    , toLines
    , toLastLine
    , onError
    , env_SCRIPT_DIR
    , shellEscape
    , escapeAll
    , wordsQuoted
    , compactWordsQuoted
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
import Streamly.Internal.Unicode.String (str)
import System.IO.Unsafe (unsafePerformIO)

import qualified System.Exit as Exit (die)
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as Parser
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import qualified Streamly.System.Sh as Sh

--------------------------------------------------------------------------------
-- String prettifying utilities
--------------------------------------------------------------------------------

wordsQuoted :: String -> [String]
wordsQuoted =
    unsafePerformIO . Stream.toList . Stream.parseMany parser . Stream.fromList

    where

    isQuote = (`elem` ['"', '\''])
    isDelimiter = (`elem` [' ', '\n'])
    parser =
        Parser.wordQuotedBy
            True
            (== '\\')
            isQuote
            isQuote
            id
            isDelimiter
            Fold.toList

-- | Trim any extra delimiters from a string while preserving the quotes
-- Delimiters : space ( ), newline (\n)
-- Quotes     : single quote ('), double quote (")
compactWordsQuoted :: String -> String
compactWordsQuoted = unwords . wordsQuoted

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- XXX Re-evaluate the names of these helpers

die :: String -> IO ()
die x = Exit.die [str|Error: #{x}|]

warn :: String -> IO ()
warn x = Exit.die [str|Warning: #{x}|]

toStdoutV :: String -> IO ()
toStdoutV cmd = putStrLn cmd >> Sh.toStdout cmd

toLines :: String -> Stream.SerialT IO String
toLines cmd = Sh.toLines Fold.toList cmd

toLastLine :: String -> IO String
toLastLine cmd = fmap fromJust (toLines cmd & Stream.last)

onError :: String -> IO () -> IO ()
onError cmd action =
    catch
      (Sh.toStdout cmd)
      (\(_ :: ProcessFailure) -> action)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

env_SCRIPT_DIR :: MonadIO m => m FilePath
env_SCRIPT_DIR = takeDirectory <$> liftIO getExecutablePath

shellEscape :: String -> String
shellEscape = concatMap f

    where

    f '\\' = "\\\\"
    f '"' = "\\\""
    -- f '\'' = "\\\'"
    f x = [x]

escapeAll :: String -> String
escapeAll = concatMap f

    where

    f '\\' = "\\\\"
    f '"' = "\\\""
    f '\'' = "\\'"
    -- f '\'' = "\\\'"
    f x = [x]
