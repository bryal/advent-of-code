module Parse
    ( Parser
    , applyParser
    , string
    , MC.space
    , MC.space1
    , int
    , many
    , MC.eol
    , sepBy
    , sepEndBy
    , word
    )
where

import Data.Void
import Data.Functor
import Text.Megaparsec
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as MCL

type Parser a = Parsec Void String a

applyParser :: Parser a -> String -> a
applyParser p src = either (error . errorBundlePretty) id (parse p "input" src)

int :: Parser Int
int = MCL.signed MC.space MCL.decimal

string :: String -> Parser ()
string s = MC.string s $> ()

word :: Parser String
word = some MC.letterChar
