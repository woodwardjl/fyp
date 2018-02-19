module Lexer where

import Data.List.Split
import qualified Types as T
import Token

rmemptytoken :: [T.Token] -> [T.Token]
rmemptytoken = filter (\(x, _) -> x /= T.Null)

rmemptyblock :: [[T.Token]] -> [[T.Token]]
rmemptyblock = filter (/= [])

rmbraceblock :: [[T.Token]] -> [[T.Token]]
rmbraceblock = filter (\xs -> xs /= [(T.Block, "{")] && xs /= [(T.Block, "}")])

rmemptyexpr :: [[T.Token]] -> [[T.Token]]
rmemptyexpr = filter (/= [])

rmemptyexpr' :: [[[T.Token]]] -> [[[T.Token]]]
rmemptyexpr' = \xs -> [rmemptyexpr x | x <- xs]

splitbytoken :: String -> [String]
splitbytoken []     = []
splitbytoken xs
    | second == ""  = [first]
    | otherwise     = first : [head second] : splitbytoken (tail second)
    where chars     = "¬`!\"£$%^&*()_-+={[]}~#@\';:/?.>,<\\| "
          first     = takeWhile (`notElem` chars) xs
          second    = dropWhile (`notElem` chars) xs

tokenize :: String -> [T.Token]
tokenize = \xs -> rmemptytoken [token x | x <- splitbytoken xs]
