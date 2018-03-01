module Parser where

import qualified Types as T

peek :: [T.Token] -> T.Token
peek []     = T.TokenTerminator
peek (x:_)  = x

toktail :: [T.Token] -> [T.Token]
toktail []      = error "shouldn't happen."
toktail (_:xs)  = xs

expr :: [T.Token] -> (T.Tree, [T.Token])
expr = addsubexpr

addsubexpr :: [T.Token] -> (T.Tree, [T.Token])
addsubexpr toks = let (multdivtree, toks') = multdivexpr toks
                  in
                    case peek toks' of
                      (T.TokenOperator op) | elem op [T.Plus, T.Minus]
                                  -> let (addsubtree, toks'') = expr $ toktail toks'
                                     in (T.AddSubNode op multdivtree addsubtree, toks'')
                      _           -> (multdivtree, toks')

multdivexpr :: [T.Token] -> (T.Tree, [T.Token])
multdivexpr toks = let (primarytree, toks') = conditionalexpr toks
                   in
                     case peek toks' of
                       T.TokenOperator op | op `elem` [T.Mult, T.Div]
                                  -> let (primarytree', toks'') = multdivexpr (toktail toks')
                                     in (T.MultDivNode op primarytree primarytree', toks'')
                       _          -> (primarytree, toks')

conditionalexpr :: [T.Token] -> (T.Tree, [T.Token])
conditionalexpr toks = let (conditionaltree, toks') = exprprimary toks
                       in
                         case peek toks' of
                           T.TokenComparison op
                                  -> let (primarytree, toks'') = multdivexpr (toktail toks') 
                                     in (T.ComparisonNode op primarytree conditionaltree, toks'')
                           _      -> (conditionaltree, toks')

exprprimary :: [T.Token] -> (T.Tree, [T.Token])
exprprimary toks = case peek toks of
                     T.TokenInt x  -> (T.LiteralNode x, toktail toks)
                     T.TokenBraceL -> let (expTree, toks') = expr (toktail toks)
                                      in (expTree, toktail toks')
                     T.TokenConditional o
                                   -> let (termTree, toks') = multdivexpr (toktail toks)
                                      in (T.ConditionalNode o termTree, toks')         
                     _             -> error $ "Parse error on token: " ++ show (peek toks)

parse' :: [[T.Token]] -> [T.Tree]
parse' = \xs -> [parse x | x <- xs]

parse :: [T.Token] -> T.Tree
parse tokens      = tree
  where (tree, _) = expr tokens
