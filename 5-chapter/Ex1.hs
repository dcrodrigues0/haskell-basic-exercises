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

-- Sem usar monoid

getTypeProductValue:: ProductEx -> Double
getTypeProductValue (ProductEx x _) = x

sumProductValue:: [ProductEx] -> Double
sumProductValue xs = foldl (+) 0 (getTypeProductValue xs)

-- showProductExTotal::[Double] -> ProductEx
-- showProductExTotal xs = ProductEx showProductExTotal xs Total
