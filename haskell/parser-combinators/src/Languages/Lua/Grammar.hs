module Languages.Lua.Grammar where

import Data.List.NonEmpty (NonEmpty)

type Name = String

newtype Chunk = Chunk Block

data Block = Block [Statement] (Maybe ReturnStatement)
    deriving Show

data Statement = 
    SemiColon |
    Assignment VarList ExprList |
    FunctionCall FunctionCallStatement |
    Label Name |
    Break |
    Goto Name |
    Do Block |
    While Expr Block |
    Repeat Block Expr |
    IfThenElse Expr Block [(Expr, Block)] (Maybe Block) |
    For Name Expr Expr (Maybe Expr) Block |
    ForIn NameList ExprList Block |
    FunctionDeclaration FunctionName FunctionBody |
    LocalFunctionDeclaration Name FunctionBody |
    LocalAssignment NameList ExprList
    deriving Show

type ReturnStatement = ExprList

data FunctionName = Name [Name] (Maybe Name)
    deriving Show

newtype VarList = VarList (NonEmpty Var)
    deriving Show

data Var = 
    VarName Name |
    IndexExpr PrefixExpr Expr |
    PropertyAccessExpr PrefixExpr Name
    deriving Show

newtype NameList = NameList (NonEmpty Name)
    deriving Show

newtype ExprList = ExprList (NonEmpty Expr)
    deriving Show

type LiteralString = String

data Expr = 
    Nil | 
    Boolean Bool | 
    Numeral Double |
    LiteralStringExpression LiteralString |
    VarargExpression |
    FunctionExpression FunctionBody |
    BinOp BinaryOperator Expr Expr |
    UnOp UnaryOperator Expr
    deriving Show

data PrefixExpr =
    Var Var |
    FunctionInvocationExpr FunctionCallStatement |
    QuotedExpr Expr
    deriving Show

data FunctionRefExpr = 
    FunctionCallExpr PrefixExpr |
    MethodCallExpr PrefixExpr Name
    deriving Show

data FunctionCallStatement = FunctionCallStatement FunctionRefExpr Args 
    deriving Show

data Args = 
    ArgsList ExprList |
    TableConstructor FieldList |
    LiteralStringArg LiteralString
    deriving Show

data FunctionBody = FunctionBody NameList Bool Block
    deriving Show

newtype Table = Table FieldList
    deriving Show

newtype FieldList = FieldList [Field]
    deriving Show

data Field = 
    NameAssign Name Expr |
    ExprAssign Expr Expr |
    IndexAssign Expr Expr
    deriving Show

data BinaryOperator = 
    Addition | -- +
    Subtraction | -- -
    Multiplication | -- *
    FloatDivision | -- /
    FloorDivision | -- //
    Modulo | -- %
    Exponentiation | -- ^
    -- bitwise operators
    BitwiseAnd | -- &
    BitwiseOr | -- |
    Xor | -- ~
    RightShift | -- >>
    LeftShift | -- <<
    -- relational operators
    Equal | -- ==
    NotEqual | -- ~=
    Less | -- <
    LessOrEqual | -- <=
    Greater | -- >
    GreaterEq | -- >=
    -- logical operators
    And | -- and
    Or | -- or
    -- misc
    Concat
    deriving Show

data UnaryOperator = Not | Length | UnaryMinus | BitwiseNot
    deriving Show
