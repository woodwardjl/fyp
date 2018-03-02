module TypeHelper where

import Helper
import qualified Types as T
import Variables

getvar :: T.Lexeme -> Int
getvar "heightl" = heightl
getvar "heightr" = heightr
getvar "factor"  = factorv
getvar "mdata"   = mdata
getvar "height"  = height
getvar x         = strtoint x

mathop :: T.Operator -> (Int -> Int -> Int)
mathop T.Plus  = (+)
mathop T.Minus = (-)
mathop T.Mult  = (*)
mathop T.Div   = (div)
_              = error "shouldn't happen."

op :: T.Lexeme -> T.Operator
op "plus"  = T.Plus
op "minus" = T.Minus
op "mult"  = T.Mult
op "div"   = T.Div
op _       = error "shouldn't happen."

conditional :: T.Lexeme -> T.Conditional
conditional "if"    = T.If
conditional "else"  = T.Else
_                   = error "rofl"

comparison :: T.Lexeme -> T.Comparison
comparison "lt"     = T.CLT
comparison "gt"     = T.CGT
comparison "lteq"   = T.CLTEQ
comparison "gteq"   = T.CGTEQ
comparison "noteq"  = T.CNOTEQ
comparison "eq"     = T.CEQ
comparison "iseq"   = T.CISEQ
comparison _        = error "shouldn't happen"

val :: T.Token -> Int
val (T.TokenInt x)     = x
val (T.TokenData _ x)  = x
val _                  = error "shouldn't happen..."
