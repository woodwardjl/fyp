module Types where

type Lexeme = String

data Tree   = AddSubNode Operator Tree Tree
              | MultDivNode Operator Tree Tree
              | UnaryNode Operator Tree
              | DataNode String Int
              | LiteralNode Int
              | ConditionalNode Conditional Tree
              | ComparisonNode Comparison Tree Tree
              deriving (Show, Eq)

data Token = TokenOperator Operator
             | TokenBraceL
             | TokenBraceR
             | TokenInt Int
             | TokenData String Int
             | TokenTerminator
             | TokenDefiner
             | TokenBlockL
             | TokenBlockR
             | TokenConditional Conditional
             | TokenComparison Comparison
             | Null
             deriving (Show, Eq)

data Operator = Plus | Minus | Mult | Div
                deriving Eq

data Comparison = CGT | CLT | CGTEQ | CLTEQ | CEQ | CNOTEQ
                  deriving Eq

data Conditional = If | Else
                   deriving Eq

instance Show Operator where
  show (Plus)    = "+"
  show (Minus)   = "-"
  show (Mult)    = "*"
  show (Div)     = "/"

instance Show Comparison where
  show (CGT)     = ">"
  show (CLT)     = "<"
  show (CGTEQ)   = ">="
  show (CLTEQ)   = "<="
  show (CEQ)     = "=="
  show (CNOTEQ)  = "!="

instance Show Conditional where
  show If    = "if"
  show Else  = "else"
