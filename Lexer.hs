module Lexer where

import Data.List.Split
import qualified Types as T
import Token
import Helper

rmemptytoken :: [T.Token] -> [T.Token]
rmemptytoken = filter (\(x, _) -> x /= T.Null)

rmemptyblock :: [[T.Token]] -> [[T.Token]]
rmemptyblock = filter (\x -> x /= [])

rmbraceblock :: [[T.Token]] -> [[T.Token]]
rmbraceblock = filter (\xs -> xs /= [(T.Block, "{")] && xs /= [(T.Block, "}")])

rmemptyexpr :: [[T.Token]] -> [[T.Token]]
rmemptyexpr = filter (/= [])

rmemptyexpr' :: [[[T.Token]]] -> [[[T.Token]]]
rmemptyexpr' = \xs -> [rmemptyexpr x | x <- xs]

countbraces :: [T.Token] -> (Int, Int)
countbraces xs = (length $ filter (== (T.Block, "{")) xs,
                  length $ filter (== (T.Block, "}")) xs)

-- @params: EntireSourceCode, TokenizedSourceCode
splitbytoken :: String -> [String]
splitbytoken []      = []
splitbytoken xs
    | second == ""   = [first]
    | otherwise      = first : [head second] : splitbytoken (tail second)
    where chars      = "¬`!\"£$%^&*()_-+={[]}~#@\';:/?.>,<\\| "
          first      = takeWhile (`notElem` chars) xs
          second     = dropWhile (`notElem` chars) xs

bracecnt :: [T.Token] -> (Int, Int)
bracecnt = \xs -> (length $ filter (== (T.Block, "{")) xs,
                   length $ filter (== (T.Block, "}")) xs)

splitbyblock' :: [[T.Token]] -> (Int, Int) -> [[T.Token]]
splitbyblock' (x:y:z:xs) cnt
  | x == tblkstart
    && y /= tblkstart
    && z /= tblkstart  = concat [x, y, z] : splitbyblock' xs cnt
  | otherwise          = x : splitbyblock' (y:z:xs) cnt
  where tblkstart      = [(T.Block, "{")]
splitbyblock' xs _     = xs

splitbyblock :: [T.Token] -> (Int, Int) -> [[T.Token]]
splitbyblock xs cnt = splitbyblock'
                      [x | x <- split (oneOf [(T.Block, "{"), (T.Block, "}")]) xs] cnt

tokenize :: String -> [T.Token]
tokenize = \xs -> rmemptytoken [token x | x <- splitbytoken xs]
