module AST where

import Data.Tree.Pretty
import qualified Data.Tree as PrettyT
import qualified Types     as T

astbuild :: [T.Tree] -> [String]
astbuild []       = []
astbuild (x:xs)   = drawVerticalTree (astconvert x) : astbuild xs  

astconvert :: T.Tree -> PrettyT.Tree String
astconvert (T.LiteralNode x)          = PrettyT.Node ("(" ++ show x ++ ")") []
astconvert (T.DataNode xs x)          = PrettyT.Node("(" ++ xs ++ ": " ++
                                                     show x ++ ")") []
astconvert (T.UnaryNode o r)          = PrettyT.Node (show o)
                                        [astconvert r]
astconvert (T.AddSubNode o l r)       = PrettyT.Node ("(" ++ show o ++ ")")
                                        [astconvert l, astconvert r]
astconvert (T.MultDivNode o l r)      = PrettyT.Node ("(" ++ show o ++ ")")
                                        [astconvert l, astconvert r]
astconvert (T.ConditionalNode o l r r')     = PrettyT.Node ("(if-else)")
                                         [astconvert l, astconvert r, astconvert r']
astconvert (T.ComparisonNode o r l)   = PrettyT.Node ("(" ++ show o ++ ")")
                                        [astconvert l, astconvert r]
astconvert (T.GroupingNode _ t _)     = astconvert t
astconvert (T.BlockNode _ t _)        = astconvert t
astconvert (T.LeafNode)               = PrettyT.Node [] []
