module Parse (Parser, applyParser, string, MC.space, MC.space1, int) where

import Data.Void
import Data.Functor
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as MCL

type Parser a = M.Parsec Void String a

applyParser :: Parser a -> String -> a
applyParser p src =
    either (error . M.errorBundlePretty) id (M.parse p "input" src)

int :: Parser Int
int = MCL.signed MC.space MCL.decimal

string :: String -> Parser ()
string s = MC.string s $> ()
