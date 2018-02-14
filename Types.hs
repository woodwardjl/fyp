module Types where

type Token      = (TokenType, Lexeme)
type Lexeme     = String

data TokenType  = Keyword | Type | Operator | Literal | Block | Null
                  | Accessor | Definer | Terminator
                  deriving (Show, Eq)

data Keyword    = ConditionalKeyword | DataKeyword | PrimaryKeyword
                  deriving (Show, Eq)

data Type       = Int
                  deriving (Show, Eq)

data Operator   = GT | LT | GTEQ | LTEQ | NOTEQ | PLUS | NEG | NOT | MULT | DIV
                  deriving (Show, Eq)
