module Types where

type Token      = (TokenType, Lexeme)
type Lexeme     = String

data TokenType  = Keyword | Type | Operator | Literal | Block | Null | Accessor | Definer | Terminator
                  deriving (Show, Eq)

data Keyword    = ConditionalKeyword | DataKeyword | PrimaryKeyword


