module Lexer where

import Data.List.Split
import qualified Types as T
import Token

rmemptytoken :: [T.Token] -> [T.Token]
rmemptytoken = filter (/= T.Null)

rmemptyblock :: [[T.Token]] -> [[T.Token]]
rmemptyblock = filter (/= [])

tokensbuild :: String -> [[T.Token]]
tokensbuild = rmemptyblock
              . splitWhen (== T.TokenTerminator)
              . rmemptytoken
              . tokenize

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


