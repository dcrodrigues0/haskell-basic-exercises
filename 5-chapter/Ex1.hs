module Ex1 where
import Data.Monoid
import Data.Semigroup

import Data.Function

data TypeProduct = Office | Informatics | Book | Movie | Total deriving Show
data ProductEx = ProductEx { value :: Double, tp :: TypeProduct} | NA deriving Show

instance Semigroup ProductEx where
  (<>) (ProductEx value1 typeProduct1) (ProductEx value2 typeProduct2) 
    = ProductEx (value1 + value2) Total

instance Monoid ProductEx where
  mempty = NA

-- Building the same answer without monoid

getProductExValue:: ProductEx -> Double
getProductExValue (ProductEx x _) = x

sumProductValue:: [ProductEx] -> Double
sumProductValue xs = xs 
    & map getProductExValue
    & foldl (+) 0    

-- Using Monoid i can calc products value in using one Function