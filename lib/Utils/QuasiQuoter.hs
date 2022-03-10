{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Description: Template Haskell code. This code needs to be in a separate
-- module because of GHC stage restriction.

-- Some of the code in this module is inspired by neat-interpolation by
-- nikita-volkov.

module Utils.QuasiQuoter
    ( line
    , multi
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Monad (void)
import Control.Applicative (Alternative(..))
import Control.Monad.Catch (MonadCatch)
import Data.Char (isAlphaNum)
import Streamly.Internal.Data.Parser (Parser)

import Language.Haskell.TH.Quote
import Language.Haskell.TH

import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import qualified Streamly.Internal.Data.Parser as Parser
import qualified Streamly.Internal.Data.Fold as Fold

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

type Line = [LineContent]

data LineContent
    = LineContentText String
    | LineContentIdentifier String
    deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

replace :: Eq a => a -> a -> [a] -> [a]
replace i j = map replaceF

    where

    replaceF x
        | x == i = j
        | otherwise = x

-- We can merge line and multi by making parsing smarter
line :: QuasiQuoter
line =
    QuasiQuoter
        { quoteExp = smartStringE (replace '\n' ' ' . stripAndPad)
        , quotePat = notSupported
        , quoteType = notSupported
        , quoteDec = notSupported
        }

    where

    notSupported = error "line: Not supported."

multi :: QuasiQuoter
multi =
    QuasiQuoter
        { quoteExp = smartStringE (++ " ")
        , quotePat = notSupported
        , quoteType = notSupported
        , quoteDec = notSupported
        }

    where

    notSupported = error "multi: Not supported."

-- | Clear all new lines and trim the input
-- This has a small workaround for parser alternative instance bug
-- We add a space in the end
stripAndPad :: String -> String
stripAndPad =
    reverse
        . (' ' :)
        . dropWhile isSpaceOrNewLine . reverse . dropWhile isSpaceOrNewLine

    where

    isSpaceOrNewLine x = x == ' ' || x == '\n'

smartStringE :: (String -> String) -> String -> Q Exp
smartStringE preprocess ln =
    case Stream.parse lineParser (Stream.fromList (preprocess ln)) of
        Left _ -> fail "Some error has occured."
        Right xs ->
            -- We need remove the ' ' at the end that we add for the hack.
            if last xs == LineContentText " "
            then lineExp (init xs)
            else lineExp xs

lineExp :: Line -> Q Exp
lineExp xs = appE [| concat |] $ listE $ map contentExp xs

contentExp :: LineContent -> Q Exp
contentExp (LineContentText text) = stringE text
contentExp (LineContentIdentifier name) = do
    valueName <- lookupValueName name
    case valueName of
        Just vn -> varE vn
        Nothing -> fail $ "Value `" ++ name ++ "` is not in scope"

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

-- streamly-0.8.0 does not expose char parser
-- We need to get all the utils working with streamly-0.8.1 for migration
charP :: MonadCatch m => Char -> Parser m Char Char
charP c = Parser.satisfy (== c)

-- streamly-0.8.0 does not expose alphaNum parser
-- We need to get all the utils working with streamly-0.8.1 for migration
alphaNumP :: MonadCatch m => Parser m Char Char
alphaNumP = Parser.satisfy isAlphaNum

lineParser :: MonadCatch m => Parser m Char Line
lineParser = Parser.many content Fold.toList

    where

    identifierSimple =
        Parser.some (alphaNumP <|> charP '\'' <|> charP '_') Fold.toList
    identifierInBraces = charP '{' *> identifierSimple <* charP '}'
    identifier =
        fmap LineContentIdentifier
            $ charP '$' *> (identifierInBraces <|> identifierSimple)
    escapedDollar = fmap (LineContentText . (: [])) $ charP '$' *> charP '$'
    -- "Parser.count" is undefined. The current implementation eats the
    -- malformed '$' instead of erroring out. This should be fixed if 'count' is
    -- used.
    -- escapedDollar =
    --     fmap LineContentText
    --         $ charP '$' *> Parser.count 1 (charP '$') Fold.toList
    anySingle = Parser.satisfy (const True)
    end =
        void (Parser.lookAhead escapedDollar)
            <|> void (Parser.lookAhead identifier)
            <|> Parser.eof
    contentText = LineContentText <$> Parser.manyTill anySingle end Fold.toList
    content = escapedDollar <|> identifier <|> contentText
