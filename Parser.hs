module Parser where

import qualified Types as T
import Variables
import Helper

getvar :: T.Lexeme -> Int
getvar "heightl" = heightl
getvar "heightr" = heightr
getvar "factor"  = factor
getvar "mdata"   = mdata
getvar "height"  = height
getvar x         = strtoint x

op :: T.Operator -> (Int -> Int -> Bool)
op T.GT     = (>)
op T.LT     = (<)
op T.GTEQ   = (>=)
op T.LTEQ   = (<=)
op T.NOTEQ  = (/=)
op _        = error "shouldn't happen."

mathop :: T.MathOperator -> (Int -> Int -> Int)
mathop T.PLUS = (+)
mathop T.NEG  = (-)
mathop T.MULT = (*)
mathop T.DIV  = (div)
_             = error "shouldn't happen."

optomathtype :: T.Lexeme -> T.MathOperator
optomathtype "plus"  = T.PLUS
optomathtype "neg"   = T.NEG
optomathtype "mult"  = T.MULT
optomathtype "div"   = T.DIV
optomathtype _       = error "shouldn't happen."

optotype :: T.Lexeme -> T.Operator
optotype "lt"     = T.LT
optotype "gt"     = T.GT
optotype "gteq"   = T.GTEQ
optotype "lteq"   = T.LTEQ
optotype "noteq"  = T.NOTEQ
optotype "not"    = T.NOT
optotype _        = error "shouldn't happen."

math :: (Int -> Int -> Int) -> T.Lexeme -> T.Lexeme -> Int
math o x y
  | isdatakeyword x && isdatakeyword y        = varx `o` vary
  | not (isdatakeyword x) && isdatakeyword y  = strx `o` vary
  | isdatakeyword x && not (isdatakeyword y)  = varx `o` stry
  | otherwise                                 = strx `o` stry
  where strx = strtoint x
        stry = strtoint y
        varx = getvar x
        vary = getvar y

eval :: [T.Token] -> [T.Expr]
eval []                             = []
eval ((T.Terminator, _):xs)         = eval xs
eval ((_, x):(_, "plus"):(_, z):xs) = T.AddSub T.PLUS x z : eval xs
eval ((_, x):(_, "neg"):(_, z):xs)  = T.AddSub T.NEG x z : eval xs
eval ((_, x):(_, "mult"):(_, z):xs) = T.MultDiv T.MULT x z : eval xs
eval ((_, x):(_, "div"):(_, z):xs)  = T.MultDiv T.DIV x z : eval xs
eval ((_, "if"):x:(_, y):z:xs)      = T.If (eval [x]) (optotype y) (eval [z]) :
                                      eval (drop 2 xs)
eval ((T.Literal, x):xs)            = T.Value (T.IntValue (getvar x)) : eval xs
eval ((T.Keyword, x):xs)            = (if isprimarykeyword x
                                       then T.Function x
                                       else T.Empty) : eval xs
eval _                              = []
