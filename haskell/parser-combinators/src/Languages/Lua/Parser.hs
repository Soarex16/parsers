module Languages.Lua.Parser where

import Data.Void
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text
import Text.Megaparsec.Char

test p s = parseTest p (pack s)

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
    space1
    (L.skipLineComment (pack "--"))
    (L.skipBlockComment (pack "--[[") (pack "--]]"))

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser ()
symbol s = () <$ (L.symbol sc . pack) s

kw :: String -> Parser Text
kw k = lexeme ((string . pack) k <* notFollowedBy alphaNumChar)

constant :: a -> String -> Parser a
constant c = (c <$) . kw

comma :: Parser ()
comma = lexeme (symbol ",")

dot :: Parser ()
dot = lexeme (symbol ".")

colon = lexeme (symbol ":")

semicolon = lexeme (symbol ";")