module Lexer where

import Data.List.Split
import qualified Types as T
import Token
import Helper

rmemptytoken :: [T.Token] -> [T.Token]
rmemptytoken = filter (/= T.Null)

rmemptyblock :: [[T.Token]] -> [[T.Token]]
rmemptyblock = filter (/= [])

splittokens :: [T.Token] -> [[T.Token]]
splittokens xs = splitkeepdelim xs T.TokenBraceL 0

tokensbuild :: String -> [[T.Token]]
tokensbuild = rmemptyblock
              . splitWhen (==T.Null)
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


