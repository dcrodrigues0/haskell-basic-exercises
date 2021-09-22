module Ex7 where

data Tree a = NA | Leaf a | Branch a (Tree a) (Tree a) deriving Show

mapp :: Tree e a -> [a]
mapp NA = []
mapp Leaf x = x 
mapp Branch x y z = mapp 1 ++ [x] ++ mapp z
