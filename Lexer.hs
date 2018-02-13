module Lexer where

import Data.List.Split
import Helper
import qualified Types as T
import Token

rmemptytoken :: [T.Token] -> [T.Token]
rmemptytoken = filter (\(x, _) -> x /= T.Null)

rmemptyblock :: [[T.Token]] -> [[T.Token]]
rmemptyblock = filter (\x -> x /= [])

rmbraceblock :: [[T.Token]] -> [[T.Token]]
rmbraceblock = filter (\xs -> xs /= [(T.Block, "{")] && xs /= [(T.Block, "}")])

rmterminatorblock :: [[T.Token]] -> [[T.Token]]
rmterminatorblock = filter (\xs -> xs /= [(T.Terminator, ";")])

rmterminatorblock' :: [[[T.Token]]] -> [[[T.Token]]]
rmterminatorblock' = \xs -> [rmterminatorblock x | x <- xs]

rmemptyexpr :: [[T.Token]] -> [[T.Token]]
rmemptyexpr = filter (/= [])

rmemptyexpr' :: [[[T.Token]]] -> [[[T.Token]]]
rmemptyexpr' = \xs -> [rmemptyexpr x | x <- xs]

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
splitbyblock :: [T.Token] -> [[T.Token]]
splitbyblock []     = []
splitbyblock (x:xs) = [x] 
                      : (takeWhile (\(y, _) -> y /= T.Block) xs)
                      : splitbyblock (dropWhile (\(y, _) -> y /= T.Block) xs)

-- @params: TokenList, TokenList                      
splitbystatement :: [T.Token] -> [[T.Token]]
splitbystatement = \xs -> splitWhen (== (T.Terminator, ";")) xs

-- @params: TokenListInBlockList, TOkenListInExpressionListInBlockList
splitbystatement' :: [[T.Token]] -> [[[T.Token]]]
splitbystatement' = \xs -> [splitbystatement x | x <- xs]

tokenize :: String -> [T.Token]
tokenize = \xs -> rmemptytoken [token x | x <- splitbytoken xs]
