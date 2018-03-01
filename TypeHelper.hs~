module Operators where

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

