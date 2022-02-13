module Languages.Lua.Parser_own where
    
import Parser.Combinators
import Control.Applicative
import Languages.Lua.Grammar

fieldsep = char ',' <|> char ';'

unop = liftA2 UnOp parseOp expr
    where parseOp = unaryOperator [
            ("not", Not),
            ("#", Length),
            ("-", UnaryMinus),
            ("~", BitwiseNot)
            ]

expr = undefined 