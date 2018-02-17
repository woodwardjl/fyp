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

-- splitbyblock'' :: [[T.Token]] -> [[T.Token]]
-- splitbyblock'' xs =splitbyblock' $ splitbyblock'  $ splitbyblock'  $ splitbyblock' $ splitbyblock' xs
--   -- | [(T.Block, "}")] `elem` xs' = splitbyblock'' xs
--   -- | otherwise = xs'
--   -- where xs' = splitbyblock' xs
-- -- splitbyblock'' xs
-- --   | [(T.Block, "}")] `elem` xs' = splitbyblock'' xs
-- --   | otherwise = xs'
-- --   where xs' = splitbyblock' xs

-- splitbyblock' :: [[T.Token]] -> [[T.Token]]
-- splitbyblock' [] = []
-- splitbyblock' (x:y:xs)
--   | (y == [(T.Block, "}")] && x /= [(T.Keyword, "begin")])
--     || x == [(T.Block, "{")] = concat [x, y] : splitbyblock' xs
--   | length y > 0 && head y == (T.Block, "}") = concat [x, y] : splitbyblock' xs
--   | length x > 0 && head x == (T.Block, "{") && last x /= (T.Block, "}")
--     = concat [x, y] : splitbyblock' xs
--   | otherwise = x : splitbyblock' (y:xs)
-- splitbyblock' xs = xs  

-- splitbyblock :: [T.Token] -> [[T.Token]]
-- splitbyblock = \xs -> splitbyblock'' $ split (oneOf [(T.Block, "{"), (T.Block, "}")]) xs

tokenize :: String -> [T.Token]
tokenize = \xs -> rmemptytoken [token x | x <- splitbytoken xs]
