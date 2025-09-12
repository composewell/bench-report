{-# LANGUAGE ScopedTypeVariables #-}

module Utils
    ( die
    , warn
    , toStdout
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
import Data.Functor.Identity (runIdentity)
import Data.Maybe (fromJust)
import System.FilePath (takeDirectory)
import System.Environment (getExecutablePath)
import Streamly.System.Process (ProcessFailure)
import Streamly.Unicode.String (str)

import qualified System.Exit as Exit (die)
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Parser as Parser (wordWithQuotes)
import qualified Streamly.System.Process as Process

--------------------------------------------------------------------------------
-- String prettifying utilities
--------------------------------------------------------------------------------

wordsQuoted :: String -> [String]
wordsQuoted =
    runIdentity
        . Stream.fold Fold.toList
        . fmap (either (error . show) id)
        . Stream.parseMany parser
        . Stream.fromList

    where

    isDelimiter = (`elem` [' ', '\n'])

    toRQuote x =
        case x of
            '"' -> Just x
            '\'' -> Just x
            _ -> Nothing

    trEsc q x = if q == x then Just x else Nothing

    parser =
        Parser.wordWithQuotes
            True
            trEsc
            '\\'
            toRQuote
            isDelimiter
            Fold.toList

-- | Trim any extra delimiters from a string while preserving the quotes
-- Delimiters : space ( ), newline (\n)
-- Quotes     : single quote ('), double quote (")
compactWordsQuoted :: String -> String
compactWordsQuoted = unwords . wordsQuoted

--------------------------------------------------------------------------------
-- Shell Helpers
--------------------------------------------------------------------------------

{-# INLINE interpreter #-}
interpreter :: (FilePath -> [String] -> a) -> String -> a
interpreter f cmd = f "/bin/sh" ["-c", cmd]

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- XXX Re-evaluate the names of these helpers

toStdout :: String -> IO ()
toStdout = interpreter Process.toStdout

die :: String -> IO ()
die x = Exit.die [str|Error: #{x}|]

warn :: String -> IO ()
warn x = Exit.die [str|Warning: #{x}|]

toStdoutV :: String -> IO ()
toStdoutV cmd = putStrLn cmd >> toStdout cmd

toLines :: String -> Stream.Stream IO String
toLines cmd = interpreter (Process.toLines Fold.toList) cmd

toLastLine :: String -> IO String
toLastLine cmd = fmap fromJust (toLines cmd & Stream.fold Fold.latest)

onError :: String -> IO () -> IO ()
onError cmd action =
    catch
      (toStdout cmd)
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
