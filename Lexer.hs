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

peek :: [T.Token] -> T.Token
peek []     = T.TokenTerminator
peek (x:_)  = x

toktail :: [T.Token] -> [T.Token]
toktail []      = error "shouldn't happen."
toktail (_:xs)  = xs

expression :: [T.Token] -> (T.Tree, [T.Token])
expression toks = 
   let (termTree, toks') = term toks
   in
      case peek toks' of
         (T.TokenOperator op) | elem op [T.Plus, T.Minus] -> 
                                let (exTree, toks'') = expression (toktail toks') 
                                in (T.AddSubNode op termTree exTree, toks'')
         _ -> (termTree, toks')

term :: [T.Token] -> (T.Tree, [T.Token])
term toks = 
   let (facTree, toks') = factor toks
   in
      case peek toks' of
         (T.TokenOperator op) | elem op [T.Mult, T.Div] ->
                                let (termTree, toks'') = term (toktail toks') 
                                in (T.MultDivNode op facTree termTree, toks'')
         _ -> (facTree, toks')

factor :: [T.Token] -> (T.Tree, [T.Token])
factor toks = 
   case peek toks of
      (T.TokenInt x)     -> (T.LiteralNode x, toktail toks)
      (T.TokenOperator op) | elem op [T.Plus, T.Minus] -> 
                             let (facTree, toks') = factor (toktail toks) 
                             in (T.UnaryNode op facTree, toks')
      T.TokenBraceL      -> 
         let (expTree, toks') = expression (toktail toks)
         in (expTree, toktail toks')
      T.TokenData xs x   -> (T.DataNode xs x, toktail toks)
      _ -> error $ "Parse error on token: " ++ show toks

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
  where (tree, _) = expression tokens
