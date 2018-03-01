module Parser where

import Data.Tree.Pretty
import qualified Types     as T
import qualified Data.Tree as PrettyT

peek :: [T.Token] -> T.Token
peek []     = T.TokenTerminator
peek (x:_)  = x

toktail :: [T.Token] -> [T.Token]
toktail []      = error "shouldn't happenXDXD."
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
                     T.TokenInt x -> (T.LiteralNode x, toktail toks)
                     T.TokenBraceL
                                  -> let (expTree, toks') = expr (toktail toks)
                            in (expTree, toktail toks')
                     T.TokenConditional o
                                  -> let (termTree, toks') = multdivexpr (toktail toks)
                            in (T.ConditionalNode o termTree, toks')         
                     _            -> error $ "Parse error on token: " ++ show (peek toks)

parse' :: [[T.Token]] -> [T.Tree]
parse' = \xs -> [parse x | x <- xs]

parse :: [T.Token] -> T.Tree
parse tokens      = tree
  where (tree, _) = expr tokens

astbuild :: [T.Tree] -> [String]
astbuild []       = []
astbuild (x:xs)   = drawVerticalTree (astconvert x) : astbuild xs  

astconvert :: T.Tree -> PrettyT.Tree String
astconvert (T.LiteralNode x)        = PrettyT.Node ("lit: (" ++ show x ++ ")") []
astconvert (T.DataNode xs x)        = PrettyT.Node("(" ++ xs ++ ": " ++ show x ++ ")") []
astconvert (T.UnaryNode o r)        = PrettyT.Node (show o)
                                      [astconvert r]
astconvert (T.AddSubNode o l r)     = PrettyT.Node ("addsub: (" ++ show o ++ ")")
                                      [astconvert l, astconvert r]
astconvert (T.MultDivNode o l r)    = PrettyT.Node ("(" ++ show o ++ ")")
                                      [astconvert l, astconvert r]
astconvert (T.ConditionalNode o r)  = PrettyT.Node ("(" ++ show o ++ ")")
                                      [astconvert r]
astconvert (T.ComparisonNode o r l) = PrettyT.Node ("(" ++ show o ++ ")")
                                      [astconvert l, astconvert r]
