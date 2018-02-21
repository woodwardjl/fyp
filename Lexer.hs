module Lexer where

import Data.List.Split
import qualified Types as T
import Token
import Operators

rmemptytoken :: [T.Token] -> [T.Token]
rmemptytoken = filter (/= T.Null)

rmemptyblock :: [[T.Token]] -> [[T.Token]]
rmemptyblock = filter (/= [])

splitbyterminator :: [T.Token] -> [[T.Token]]
splitbyterminator = splitWhen (==T.TokenTerminator)

peek :: [T.Token] -> T.Token
peek []     = T.TokenTerminator
peek (x:_)  = x

toktail :: [T.Token] -> [T.Token]
toktail []      = error "shouldn't happen."
toktail (_:xs)  = xs

expr :: [T.Token] -> (T.Tree, [T.Token])
expr tokens =
  case peek tokens' of
    (T.TokenOperator op)
      | op `elem` [T.Plus, T.Minus] -> (T.AddSubNode op primaryTree exprTree, tokens'')
    _ -> (primaryTree, tokens')
  where (primaryTree, tokens') = term tokens
        (exprTree, tokens'') = expr (toktail tokens')

term :: [T.Token] -> (T.Tree, [T.Token])
term tokens =
  case peek tokens' of
    (T.TokenOperator op)
      | op `elem` [T.Mult, T.Div] -> (T.MultDivNode op primaryTree termTree, toks'')
    _                             -> (primaryTree, tokens')
  where (termTree, toks'')      = term tokens'
        (primaryTree, tokens')  = primary tokens
        

primary :: [T.Token] -> (T.Tree, [T.Token])
primary tokens = 
  case peek tokens of
    T.TokenInt x                    -> (T.LiteralNode x, toktail tokens)
    T.TokenData x                   -> (T.LiteralNode x, toktail tokens)
    T.TokenOperator op
      | op `elem` [T.Plus, T.Minus] -> (T.UnaryNode op primaryTree, opTokens)
    T.TokenBraceL                   -> if peek exprTokens /= T.TokenBraceR
                                       then error "right brace??"
                                       else (exprTree, exprTokens)
    _                               -> error "invalid tokens XD"
  where (primaryTree, opTokens) = primary $ toktail tokens
        (exprTree, exprTokens)  = expr $ toktail tokens

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

parse' :: [[T.Token]] -> [T.Tree]
parse' = \xs -> [parse x | x <- xs]

parse :: [T.Token] -> T.Tree
parse tokens = tree
  where (tree, _) = expr tokens
