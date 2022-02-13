module Parser.Combinators where
import Parser
import Parser.Error
import Data.Char
import Control.Applicative
import Data.List
import Data.Foldable

satisfy :: (Token -> Bool) -> (Token -> String) -> Parser String Token
satisfy pred err = Parser f
    where f [] = Left "Empty input"
          f (t:ts)
              | pred t = Right (ts, t)
              | otherwise = Left (err t)

char :: Token -> Parser String Token
char c = satisfy (== c) (unexpectedToken c)

string str = Parser f
    where f s = case stripPrefix str s of
            Nothing -> Left (unexpectedInput str s)
            Just s' -> Right (s, s')

notChar c = satisfy (/= c)

digit = satisfy isDigit (unexpectedInput "digit")

space = satisfy isSeparator (unexpectedInput "space symbol")

between :: Parser e a -> Parser e b -> Parser e r -> Parser e r
between l p r = l *> r <* p

integer :: Parser String Integer
integer = read <$> some digit

choice :: [Parser String r] -> Parser String r
choice = asum

unaryOperator opTable = choice $ (\(op, c) -> c <$ string op) <$> opTable

doubleQuote = char '"'

quoted = between doubleQuote doubleQuote

nonQuote = notChar '"' (unexpectedInput "non quote symbol")

stringLiteral = quoted (many nonQuote)