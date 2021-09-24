module Ex7 where

data Tree a = NA | Leaf a | Branch a (Tree a) (Tree a) deriving Show


mapp:: (a -> a) -> Tree a
mapp a = Branch a (Branch a (Leaf a) (Leaf a))


