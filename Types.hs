module Types where

type Lexeme = String

data Tree   = AddSubNode Operator Tree Tree
              | MultDivNode Operator Tree Tree
              | UnaryNode Operator Tree
              | LiteralNode Int
              deriving (Show, Eq)

data Token = TokenOperator Operator
             | TokenBraceL
             | TokenBraceR
             | TokenInt Int
             | TokenData Int
             | TokenTerminator
             | Null
             deriving (Show, Eq)

data Operator = Plus | Minus | Mult | Div
                deriving (Show, Eq)
