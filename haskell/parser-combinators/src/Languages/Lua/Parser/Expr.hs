module Languages.Lua.Parser.Expr where

import Languages.Lua.Grammar
import Languages.Lua.Parser
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Maybe
import Data.List.NonEmpty
import Control.Applicative (liftA2, liftA3)
import Languages.Lua.Parser.Util
import Parser.Combinators (stringLiteral)

expr :: Parser Expr
expr = undefined

nil :: Parser Expr
nil = Nil <$ kw "nil"

bool :: Parser Expr
bool = Boolean <$> choice (uncurry constant <$> [(True, "true"), (False, "false")])

numeral :: Parser Expr
numeral = Numeral <$> lexeme L.float

literalString :: Parser LiteralString
literalString = lexeme parseString
    where parseString = char '\"' *> manyTill L.charLiteral (char '\"')

vararg :: Parser Expr
vararg = constant VarargExpression "..."

functiondef :: Parser Expr
functiondef = FunctionExpression <$> (kw "function" *> funcbody)

parentesis :: Parser a -> Parser a
parentesis = between (symbol "(") (symbol ")")

name :: Parser Name
name = lexeme $ (:) <$> (letterChar <|> char '_') <*> many alphaNumChar

namelist :: Parser NameList
namelist = mkNonEmpty NameList <$> sepBy1 name comma -- fromJust is pretty bad

varlist :: Parser VarList
varlist = mkNonEmpty VarList <$> sepBy1 var comma

exprlist :: Parser ExprList
exprlist = mkNonEmpty ExprList <$> sepBy1 expr comma

var :: Parser Var
var = choice [
    VarName <$> name,
    liftA2 IndexExpr prefixExpr (between (symbol "[") (symbol "]") expr),
    liftA2 PropertyAccessExpr prefixExpr (dot *> name)
    ]

prefixExpr :: Parser PrefixExpr
prefixExpr = choice [
    Var <$> var,
    FunctionInvocationExpr <$> functionInvocation,
    QuotedExpr <$> parentesis expr
    ]

functionInvocation :: Parser FunctionCallStatement
functionInvocation = liftA2 FunctionCallStatement functionRef args

functionRef :: Parser FunctionRefExpr
functionRef = choice [
    FunctionCallExpr <$> prefixExpr,
    liftA2 MethodCallExpr prefixExpr (colon *> name)
    ]

args :: Parser Args
args = choice [
    ArgsList <$> parentesis exprlist,
    LiteralStringArg <$> literalString
    ]

funcbody = liftA3 FunctionBody namelist varargMarker block
    where varargMarker = True <$ vararg <|> pure False
          varargMarker :: Parser Bool

statement :: Parser Statement
statement = undefined

block :: Parser Block
block = liftA2 Block (many statement) (optional returnStatement)

semicolonStatement :: Parser ()
semicolonStatement = () <$ semicolon

assignmentStatement :: Parser Statement
assignmentStatement = liftA2 Assignment (varlist <* symbol "=") exprlist

returnStatement :: Parser ReturnStatement
returnStatement = kw "return" *> exprlist <* optional semicolon

-- functionCallStatement :: Parser FunctionCallStatement
-- functionCallStatement = liftA2 FunctionCallStatement nameExpr args

