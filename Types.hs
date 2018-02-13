module Types where

type MToken     = (MTokenType, Lexeme)
type Lexeme     = String

data MTokenType = MKeyword | MDataKeyword | MFunction | MType | MLiteral
                  | MBlockS | MBlockF | MOperator | MAssignment | MUndefined
                  | MEmpty | MAccessor | MTerminator | MConditionalKeyword
                  deriving (Show, Eq)

data MType      = MInt 
                  deriving (Show, Eq)

data Expr       = MExprAddSubtract | MExprMultDiv | | MExprUnary | MExprGrouping 
                  | MExprValue | MExprLiteral | MExprKeywordPrimary | MExprKeywordData
                  | MExprKeywordConditional | MExprType | MExprOperator
                  deriving (Show, Eq)

data Stmt       = MDefineStmt | MAssignmentStmt | MConditionalStmt | MBlockStmt
                  deriving (Show, Eq)