module Bst where

import Helper
import Data.Tree.Pretty
import qualified Data.Tree as PrettyT

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

tprint :: Show a => Tree a -> IO ()
tprint  = putStrLn . (drawVerticalTree . tconvert)

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert x (Leaf)       = (Node Leaf x Leaf)
tinsert x (Node l y r) = case compare x y of
                             LT -> Node (tinsert x l) y r
                             EQ -> (Node l x r)
                             GT -> Node l y (tinsert x r)

tsearch :: Ord a => a -> Tree a -> Bool
tsearch _ (Leaf)        = False
tsearch x (Node l y r)  = case compare x y of
                             LT -> tsearch x l
                             EQ -> True
                             GT -> tsearch x r

tremove :: Ord a => a -> Tree a -> Tree a
tremove _ Leaf                            = Leaf
tremove x (Node Leaf y Leaf) | x == y     = Leaf
                             | otherwise  = Node Leaf y Leaf
tremove x (Node Leaf y r)    | x == y     = r
                             | otherwise  = Node Leaf y (tremove x r)
tremove x (Node l y Leaf)    | x == y     = l
                             | otherwise  = Node (tremove x l) y Leaf
tremove x (Node l y r)       = case compare x y of
                                   LT -> Node (tremove x l) y r
                                   EQ -> Node l minr (tremove minr r)
                                   GT -> Node l y (tremove x r)
    where minr = tgetminr r

trotater :: Ord a => a -> Tree a -> Tree a
trotater _ (Leaf)          = Leaf
trotater _ (Node Leaf y r) = Node Leaf y r
trotater x (Node l y r)    = case compare x y of
                              LT -> Node (trotater x l) y r
                              EQ -> Node l' y' r'
                              GT -> Node l y (trotater x r)
    where l' = tgetchildl l
          y' = tgetvalue l
          r' = Node (tgetchildr l) y r

trotatel :: Ord a => a -> Tree a -> Tree a
trotatel _ (Leaf)          = Leaf
trotatel _ (Node r y Leaf) = Node r y Leaf
trotatel x (Node l y r)    = case compare x y of
                              LT -> Node (trotatel x l) y r
                              EQ -> Node l' y' r'
                              GT -> Node l y (trotatel x r)
    where l' = Node l y (tgetchildl r)
          y' = tgetvalue r
          r' = tgetchildr r

tconvert :: Show a => Tree a -> PrettyT.Tree String
tconvert Leaf              = PrettyT.Node "LEAF" []
tconvert (Node l y r)      = PrettyT.Node ("(" ++ show y ++ ")")
                             [tconvert l, tconvert r]

tbalance :: Ord a => Tree a -> Tree a
tbalance (Leaf)            = Leaf
tbalance (Node l y r)      = tmake (tflatten (Node l y r))

tcountleaves :: Tree a -> Int
tcountleaves (Leaf)        = 1
tcountleaves (Node l _ r)  = (tcountleaves l) + (tcountleaves r)

tflatten :: Tree a -> [a]
tflatten (Leaf)            = []
tflatten (Node l x r)      = tflatten l ++ [x] ++ tflatten r

tmake :: Ord a => [a] -> Tree a
tmake []                   = Leaf
tmake xs                   = Node (tmake ys) root (tmake zs)
    where (ys, (root:zs))  = splitlist (rmdups xs)

tgetminr :: Tree a -> a
tgetminr Leaf              = error "leaves do not have subtrees"
tgetminr (Node Leaf y _)   = y
tgetminr (Node l _ _)      = tgetminr l

tgetchildr :: Tree a -> Tree a
tgetchildr (Leaf)          = Leaf
tgetchildr (Node _ _ r)    = r

tgetchildl :: Tree a -> Tree a
tgetchildl (Leaf )         = Leaf
tgetchildl (Node l _ _)    = l

tgetvalue :: Tree a -> a
tgetvalue (Leaf)           = error "leaves have no values"
tgetvalue (Node _ x _)     = x
