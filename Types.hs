module Types where

type Lexeme = String

data Tree   = AddSubNode Operator Tree Tree
              | MultDivNode Operator Tree Tree
              | UnaryNode Operator Tree
              | LiteralNode Int
              | DataNode String Int
              deriving (Show, Eq)

data Token = TokenOperator Operator
             | TokenBraceL
             | TokenBraceR
             | TokenInt Int
             | TokenData String Int
             | TokenTerminator
             | Null
             deriving (Show, Eq)

data Operator = Plus | Minus | Mult | Div
                deriving Eq

instance Show Operator where
  show (Plus)  = "+"
  show (Minus) = "-"
  show (Mult)  = "*"
  show (Div)   = "/"
