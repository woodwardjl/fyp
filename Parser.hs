module Parser where

import qualified Types as T
import Data.Tree.Pretty
import qualified Data.Tree as PrettyT
import System.Exit

peek :: [T.Token] -> T.Token
peek []     = T.TokenTerminator
peek (x:_)  = x

toktail :: [T.Token] -> [T.Token]
toktail []      = []
toktail (_:xs)  = xs

expr :: [T.Token] -> (T.Tree, [T.Token])
expr = addsubexpr

addsubexpr :: [T.Token] -> (T.Tree, [T.Token])
addsubexpr toks = let (multdivtree, toks') = multdivexpr toks
                  in
                    case peek toks' of
                      T.TokenOperator op | elem op [T.Plus, T.Minus]
                                  -> let (exprtree, toks'') = expr $ toktail toks'
                                     in (T.AddSubNode op multdivtree exprtree, toks'')
                      _           -> (multdivtree, toks')

multdivexpr :: [T.Token] -> (T.Tree, [T.Token])
multdivexpr toks = let (primarytree, toks') = comparisonexpr toks
                   in
                     case peek toks' of
                       T.TokenOperator op | op `elem` [T.Mult, T.Div]
                                  -> let (primarytree', toks'') = expr (toktail toks')
                                     in (T.MultDivNode op primarytree primarytree', toks'')
                       _          -> (primarytree, toks')

comparisonexpr :: [T.Token] -> (T.Tree, [T.Token])
comparisonexpr toks = let (primarytree, toks') = exprprimary toks
                       in
                         case peek toks' of
                           T.TokenComparison op
                                  -> let (primarytree', toks'') = multdivexpr (toktail toks') 
                                     in (T.ComparisonNode op primarytree' primarytree
                                        , toks'')
                           _      -> (primarytree, toks')
                                     
exprprimary :: [T.Token] -> (T.Tree, [T.Token])
exprprimary toks = case peek toks of
                     T.TokenInt x      -> (T.LiteralNode x, toktail toks)
                     T.TokenData xs x  -> (T.DataNode xs x, toktail toks)
                     T.TokenBraceL     -> let (exprtree, toks'') = expr (toktail toks)
                                          in (T.GroupingNode T.TokenBraceL exprtree T.TokenBraceR
                                         , toktail toks'')
                     T.TokenBraceR     -> let (exprtree, toks'') = expr (toktail toks)
                                          in (T.GroupingNode T.TokenBraceL exprtree T.TokenBraceR
                                         , toktail toks'')
                     T.TokenConditional o
                                       -> let (primarytree, toks'')  = exprprimary (toktail toks)
                                              (primarytree', toks''')= exprprimary (toks'')
                                              (primarytree'', _)     = exprprimary (toks''')
                                          in (T.ConditionalNode o primarytree primarytree'
                                              primarytree'', toks'')
                     t                 -> error $ "unable to parse token: " ++ show t
parse' :: [[T.Token]] -> [T.Tree]
parse' = \xs -> [parse x | x <- xs]

parse :: [T.Token] -> T.Tree
parse tokens      = tree
  where (tree, _) = expr tokens
