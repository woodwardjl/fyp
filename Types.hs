module Types where

type Token      = (TokenType, Lexeme)
type Lexeme     = String

data TokenType  = Keyword | Type | Operator | Literal | Block | Null
                  | Accessor | Definer | Terminator
                  deriving (Show, Eq)

data Literal    = IntValue Int
                  | StrValue String
                  deriving (Show, Eq)

data Operator   = MathOperator | GT | LT | GTEQ | LTEQ | NOTEQ | NOT
                  deriving (Show, Eq)

data MathOperator = PLUS | NEG | MULT | DIV
                    deriving (Show, Eq)

data Expr       = Value Literal
                  | Function String
                  | AddSub MathOperator String String
                  | MultDiv MathOperator String String
                  | If [Expr] Operator [Expr]
                  | Empty
                  deriving (Show, Eq)
