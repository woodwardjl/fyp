module Operators where

import Helper
import qualified Types as T
import Variables

getvar :: T.Lexeme -> Int
getvar "heightl" = heightl
getvar "heightr" = heightr
getvar "factor"  = factor
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

-- op :: T.Operator -> (Int -> Int -> Bool)
-- op T.GT     = (>)
-- op T.LT     = (<)
-- op T.GTEQ   = (>=)
-- op T.LTEQ   = (<=)
-- op T.NOTEQ  = (/=)
-- op _        = error "shouldn't happen."

-- optotype :: T.Lexeme -> T.Operator
-- optotype "lt"     = T.LT
-- optotype "gt"     = T.GT
-- optotype "gteq"   = T.GTEQ
-- optotype "lteq"   = T.LTEQ
-- optotype "noteq"  = T.NOTEQ
-- optotype "not"    = T.NOT
-- optotype _        = error "shouldn't happen."

-- math :: (Int -> Int -> Int) -> T.Lexeme -> T.Lexeme -> Int
-- math o x y
--   | isdatakeyword x && isdatakeyword y        = varx `o` vary
--   | not (isdatakeyword x) && isdatakeyword y  = strx `o` vary
--   | isdatakeyword x && not (isdatakeyword y)  = varx `o` stry
--   | otherwise                                 = strx `o` stry
--   where strx = strtoint x
--         stry = strtoint y
--         varx = getvar x
--         vary = getvar y
