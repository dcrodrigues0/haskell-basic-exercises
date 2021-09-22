module Ex6 where

newtype Max = Max Int deriving (Ord, Eq, Show)

instance Semigroup Max where
  (<>) (Max a) (Max b) = Max $ max a b

instance Monoid Max where
  mempty = Max 0

maxAll:: [Max] -> Max
maxAll xs = foldl (<>) mempty xs