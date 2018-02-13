module Lexer where

import Data.List.Split
import Helper
import Types
import Token

rmemptytoken :: [MToken] -> [MToken]
rmemptytoken = filter (\(x, _) -> x /= MEmpty)

rmemptyblock :: [[MToken]] -> [[MToken]]
rmemptyblock = filter (\x -> x /= [])

rmbraceblock :: [[MToken]] -> [[MToken]]
rmbraceblock = filter (\xs -> xs /= [(MBlockS, "{")] && xs /= [(MBlockF, "}")])

rmterminatorblock :: [[MToken]] -> [[MToken]]
rmterminatorblock = filter (\xs -> xs /= [(MTerminator, ";")])

rmterminatorblock' :: [[[MToken]]] -> [[[MToken]]]
rmterminatorblock' = \xs -> [rmterminatorblock x | x <- xs]

rmemptyexpr :: [[MToken]] -> [[MToken]]
rmemptyexpr = filter (/= [])

rmemptyexpr' :: [[[MToken]]] -> [[[MToken]]]
rmemptyexpr' = \xs -> [rmemptyexpr x | x <- xs]

-- @params: Tokens, StartBraceCount, EndBraceCount,
--          (StartBraceCount, EndBraceCount)                 
bracecount :: [MToken] -> Int -> Int -> (Int, Int)
bracecount [] cStart cEnd     = (cStart, cEnd)
bracecount (x:xs) cStart cEnd
    | fst x == MBlockS        = bracecount xs (cStart + 1) cEnd
    | fst x == MBlockF        = bracecount xs cStart (cEnd + 1)
    | otherwise               = bracecount xs cStart cEnd

-- @params: EntireSourceCode, TokenizedSourceCode
splitbytoken :: String -> [String]
splitbytoken []      = []
splitbytoken xs
    | second == ""   = [first]
    | otherwise      = first : [head second] : splitbytoken (tail second)
    where chars      = "¬`!\"£$%^&*()_-+={[]}~#@\';:/?.>,<\\| "
          first      = takeWhile (`notElem` chars) xs
          second     = dropWhile (`notElem` chars) xs

-- @params: TokenList, TokenListInBlockList          
splitbyblock :: [MToken] -> [[MToken]]
splitbyblock []     = []
splitbyblock (x:xs) = [x] 
                      : (takeWhile (\(y, _) -> y /= MBlockF && y /= MBlockS) xs)
                      : splitbyblock (dropWhile (\(y, _) -> y /= MBlockF && y /= MBlockS) xs)

-- @params: TokenList, TokenList                      
splitbystatement :: [MToken] -> [[MToken]]
splitbystatement = \xs -> splitWhen (== (MTerminator, ";")) xs

-- @params: TokenListInBlockList, TOkenListInExpressionListInBlockList
splitbystatement' :: [[MToken]] -> [[[MToken]]]
splitbystatement' = \xs -> [splitbystatement x | x <- xs]

tokenize :: String -> [MToken]
tokenize = \xs -> rmemptytoken [token x | x <- splitbytoken xs]
