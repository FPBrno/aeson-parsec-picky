{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aeson.Parser.Parsec.Picky
    ( string
    , object
    , array
    , number
    , bool
    , value
    ) where

import Prelude (Enum(toEnum), Int)

import Control.Applicative (pure, (<$>), (<|>), (<*), (<*>), (*>))
import Control.Monad (Monad((>>=)), sequence, void)
import Data.Bool (Bool(False, True), (&&))
-- import Data.Either (either)
import Data.Eq (Eq((/=)))
import Data.Function (flip, ($), (.))
import Data.List (concat)
import Text.Read (read)

import qualified Data.HashMap.Strict as HashMap (fromList)
import Data.Text (Text)
import qualified Data.Text as Text (pack)
-- import Data.Text.Encoding (decodeUtf8')
import qualified Data.Vector as Vector (fromList)
import Data.Scientific (Scientific)


import Data.Aeson.Types
    ( Value
        ( Object
        , Array
        , String
        , Number
        , Bool
        , Null
        )
    )
import Text.Parsec
    ( between
    , char
    , count
    , digit
    , hexDigit
    , many
    , many1
    , newline
    , option
    , satisfy
    , sepBy
    , try
    , (<?>)
    )
import qualified Text.Parsec as P (string)
import Text.Parsec.Text (Parser)

-- {{{ Helpers ----------------------------------------------------------------
newlines :: Parser ()
newlines = void $ many newline

spaces :: Parser ()
spaces = void $ many (char ' ')

commaSeparated :: Parser a -> Parser [a]
commaSeparated = flip sepBy comma where
    comma = (variant1 <|> try variant2) <* spaces
    variant1 = char ',' <* newlines
    variant2 = pickySpaces *> char ','

pickySpaces :: Parser ()
pickySpaces = newlines *> spaces

pickyBetween :: Parser a -> Parser b -> Parser c -> Parser c
pickyBetween o c = between (o <* pickySpaces) (pickySpaces *> c)
-- }}} Helpers ----------------------------------------------------------------

-- {{{ Underlaying ------------------------------------------------------------
baseString :: Parser Text
baseString = Text.pack <$> p where
    p = between (char '"') (char '"') $ many oneChar
    oneChar = raw <|> char '\\' *> quoted
    raw = satisfy (\ c -> c /= '"' && c /= '\\')
    quoted = tab <|> quot <|> revsolidus <|> solidus <|> backspace <|> formfeed
        <|> nl <|> cr <|> hexUnicode
    tab = char 't' *> pure '\t'
    quot = char '"' *> pure '"'
    revsolidus = char '/' *> pure '/'
    solidus = char '\\' *> pure '\\'
    backspace = char 'b' *> pure '\b'
    formfeed = char 'f' *> pure '\f'
    nl = char 'n' *> pure '\n'
    cr = char 'r' *> pure '\r'
    -- TODO: Implement unicode
    hexUnicode = char 'u' *> count 4 hexDigit >>= decode
    decode x = pure $ toEnum (read ('0':'x':x) :: Int)

baseNumber :: Parser Scientific
baseNumber = read . concat <$> sequence
    [ opt $ P.string "-"
    , P.string "0" <|> many1 digit
    , opt $ (:) <$> char ':' <*> many1 digit
    , opt $ concat <$> sequence
        [ P.string "e" <|> P.string "E"
        , opt $ P.string "+" <|> P.string "-"
        , many1 digit
        ]
    ]
    where
    opt = option ""
-- }}} Underlaying ------------------------------------------------------------

-- {{{ JSON Values ------------------------------------------------------------
string :: Parser Value
string = String <$> baseString <?> "JSON string"

object :: Parser Value
object = Object . HashMap.fromList <$> p <?> "JSON object" where
    p = pickyBetween (char '{') (char '}') $ commaSeparated pair
    pair = (,) <$> (baseString <?> "JSON object key (string)")
        <*> (char ':' *> pickySpaces *> value)

array :: Parser Value
array = Array . Vector.fromList <$> p <?> "JSON array" where
    p = pickyBetween (char '[') (char ']') $ commaSeparated value

number :: Parser Value
number = Number <$> baseNumber <?> "JSON number"

bool :: Parser Value
bool = Bool <$> (true <|> false) <?> "JSON bool (true|false)" where
    true = P.string "true" *> pure True
    false = P.string "false" *> pure False

null :: Parser Value
null = P.string "null" *> pure Null

value :: Parser Value
value = object <|> array <|> string <|> number <|> bool <|> null
-- }}} JSON Values ------------------------------------------------------------
