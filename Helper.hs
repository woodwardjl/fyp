module Helper where

import Data.List
import Data.Char
import qualified Types as T

splitlist :: Ord a => [a] -> ([a], [a])
splitlist []   = ([], [])
splitlist xs   = (take half xs', drop half xs')
    where half = length xs `div` 2
          xs'  = sort xs

rmdups :: Eq a => [a] -> [a]
rmdups []                   = []
rmdups (x:xs) | x `elem` xs = rmdups xs
              | otherwise   = x : rmdups xs

isspace :: Char -> Bool
isspace = \x -> x == ' '

isdigit :: String -> Bool
isdigit []     = True
isdigit (x:xs) = isDigit x && isdigit xs

isinvalid :: [T.Token] -> (Bool, String)
isinvalid []                       = (False, "")
isinvalid (x:xs) | fst x == T.Null = (True, snd x)
                 | otherwise       = isinvalid xs

isstr :: String -> Bool
isstr []     = True
isstr (x:xs) = (isAlpha x || x == '\n' || x == '\t') && isstr xs

isalphanum :: String -> Bool
isalphanum []      = True
isalphanum (x:xs)  = (isAlpha x || isDigit x) && isalphanum xs

isliteral :: String -> Bool
isliteral = \xs -> isdigit xs || isstr xs || isalphanum xs

iscomment :: String -> Bool
iscomment (x:y:xs) | isspace x             = iscomment (y:xs)
                   | x == '-' && y == '-'  = True
                   | otherwise             = False
iscomment _                                = False

iswhitespace :: String -> Bool
iswhitespace []      = True
iswhitespace (x:xs)  = isspace x && iswhitespace xs

ischar :: Char -> Char -> Bool
ischar = \x y -> x == y

str :: String -> (String, String)
str = \xs -> (takeWhile isAlpha xs, dropWhile isAlpha xs)

iskeyword :: T.Lexeme -> Bool
iskeyword = \xs -> isprimarykeyword xs || isdatakeyword xs || isconditionalkeyword xs

isprimarykeyword :: T.Lexeme -> Bool
isprimarykeyword = \xs -> xs `elem` ["begin", "if", "else", "define", "end", "not", 
                                     "print", "test", "rotatel", "rotater",
                                     "populate", "updateparents", "max"]

isdatakeyword :: T.Lexeme -> Bool                              
isdatakeyword = \xs -> xs `elem` ["adjustment", "factor", "leftchild", "rightchild", 
                                  "nodefactor", "heightl", "heightr", "data", "height"]

isconditionalkeyword :: T.Lexeme -> Bool
isconditionalkeyword = \xs -> xs `elem` ["if", "else"]

istype :: T.Lexeme -> Bool
istype = \xs -> xs `elem` ["int", "decimal", "char", "string"]

isoperator :: T.Lexeme -> Bool
isoperator = \xs -> xs `elem` ["gt", "lt", "lteq", "gteq", "or", "and", "not", "eq",
                              "plus", "div", "neg", "mult"]

isblock :: [T.Token] -> Bool
isblock = \x -> x == [(T.Block, "{")] || x == [(T.Block, "}")]

splitkeepdelim :: (Eq a) => [a] -> a -> Int -> [[a]]
splitkeepdelim [] _ _            = []
splitkeepdelim xs delim pos -- pos: 0: split token stays left; 1: split token goes right
  | delim `elem` xs = if pos == 0 then concat [taken, [delim]]
                                       : splitkeepdelim (tail dropped) delim pos
                                  else taken
                                       : [head $ dropped]
                                       : splitkeepdelim (tail $ dropped) delim pos
  | otherwise                    = [xs]
  where dropped                  = dropWhile (/= delim) xs
        taken                    = takeWhile (/= delim) xs

cnt :: (Eq a) => [a] -> [[a]] -> Int
cnt p = length . filter (== p)

takeWhileInclude :: (T.Token -> Bool) -> [T.Token] -> [T.Token]
takeWhileInclude _ []      = []
takeWhileInclude p (x:xs)  = x : if p x then takeWhileInclude p xs else []

strtoint :: String -> Int
strtoint x = read x :: Int
