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
getvar _         = error "should never occur."

math :: (Int -> Int -> Int) -> T.Lexeme -> T.Lexeme -> Int
math op x y
  | not (isoperator x || isoperator y)  = strx `op` stry
  | isoperator x  && isoperator y       = varx `op` vary
  | isoperator x                        = varx `op` stry
  | otherwise                           = strx `op` vary
  where strx                            = strtoint x
        stry                            = strtoint y
        varx                            = getvar x
        vary                            = getvar y

eval :: [T.Token] -> [T.Expr]
eval []         = [T.Other "empty"]
eval ((_, x):(_, "plus"):(_, z):(T.Terminator, ";"):xs)
                = T.AddSub T.PLUS x z: eval xs
eval ((_, x):(_, "neg"):(_, z):(T.Terminator, ";"):xs)
                = T.AddSub T.NEG x z : eval xs
eval ((_, x):(_, "mult"):(_, z):(T.Terminator, ";"):xs)
                = T.MultDiv T.MULT x z : eval xs
eval ((_, x):(_, "div"):(_, z):(T.Terminator, ";"):xs)
                = T.MultDiv T.DIV x z : eval xs
eval ((_,y):_)  = [T.Other y]

eval' :: [T.Expr] -> [T.Literal]
eval' ((T.AddSub T.NEG x y):xs)    = T.IntValue (math (-) x y) : eval' xs
eval' ((T.AddSub T.PLUS x y):xs)   = T.IntValue (math (+) x y) : eval' xs
eval' ((T.MultDiv T.MULT x y):xs)  = T.IntValue (math (*) x y) : eval' xs
eval' ((T.MultDiv T.DIV x y):xs)   = T.IntValue (math (div) x y) : eval' xs
eval' _                            = []
