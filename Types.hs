module Types where

type Lexeme = String

data Tree   = AddSubNode Operator Tree Tree
              | MultDivNode Operator Tree Tree
              | UnaryNode Operator Tree
              | DataNode Lexeme Int
              | LiteralNode Int
              | ConditionalNode Conditional Tree Tree Tree
              | ComparisonNode Comparison Tree Tree
              | GroupingNode Token Tree Token
              | BlockNode Token Tree Token
              | DefinitionNode String Tree
              | LeafNode
              deriving (Show, Eq)

data Token = TokenOperator Operator
             | TokenBraceL
             | TokenBraceR
             | TokenInt Int
             | TokenData Lexeme Int
             | TokenConditional Conditional
             | TokenComparison Comparison
             | TokenDefiner Lexeme
             | Null
             deriving (Show, Eq)

data Operator = Plus | Minus | Mult | Div
                deriving Eq

data Comparison = CGT | CLT | CGTEQ | CLTEQ | CEQ | CNOTEQ | CISEQ
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
  show (CEQ)     = "="
  show (CNOTEQ)  = "!="
  show (CISEQ)   = "=="

instance Show Conditional where
  show If    = "if"
  show Else  = "else"
