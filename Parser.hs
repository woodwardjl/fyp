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
  | isdatakeyword x && isdatakeyword y        = varx `op` vary
  | not (isdatakeyword x) && isdatakeyword y  = strx `op` vary
  | isdatakeyword x && not (isdatakeyword y)  = varx `op` stry
  | otherwise                                 = strx `op` stry
  where strx = strtoint x
        stry = strtoint y
        varx = getvar x
        vary = getvar y

eval :: [T.Token] -> [T.Expr]
eval []         = [T.Empty]
eval ((_, x):(_, "plus"):(_, z):(T.Terminator, ";"):xs)
                = T.AddSub T.PLUS x z: eval xs
eval ((_, x):(_, "neg"):(_, z):(T.Terminator, ";"):xs)
                = T.AddSub T.NEG x z : eval xs
eval ((_, x):(_, "mult"):(_, z):(T.Terminator, ";"):xs)
                = T.MultDiv T.MULT x z : eval xs
eval ((_, x):(_, "div"):(_, z):(T.Terminator, ";"):xs)
                = T.MultDiv T.DIV x z : eval xs
eval ((_,y):_)  = [T.Empty] -- For further patterns.

eval' :: [T.Expr] -> [T.Literal]
eval' ((T.AddSub T.NEG x y):xs)    = T.IntValue (math (-) x y) : eval' xs
eval' ((T.AddSub T.PLUS x y):xs)   = T.IntValue (math (+) x y) : eval' xs
eval' ((T.MultDiv T.MULT x y):xs)  = T.IntValue (math (*) x y) : eval' xs
eval' ((T.MultDiv T.DIV x y):xs)   = T.IntValue (math (div) x y) : eval' xs
eval' _                            = []
