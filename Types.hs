module Types where

type Token      = (TokenType, Lexeme)
type Lexeme     = String

data TokenType  = Keyword | Type | Operator | Literal | Block | Null
                  | Accessor | Definer | Terminator
                  deriving (Show, Eq)

data Keyword    = ConditionalKeyword | DataKeyword | PrimaryKeyword
                  deriving (Show, Eq)

data Literal    = IntValue Int
                  | StrValue String
                  deriving (Show, Eq)

data Operator   = GT | LT | GTEQ | LTEQ | NOTEQ | PLUS | NEG | NOT | MULT | DIV
                  deriving (Show, Eq)

data Expr       = I Literal
                  | AddSub Operator String String
                  | MultDiv Operator String String
                  | Empty
                  deriving (Show, Eq)
