module Ex1 where

data TypeProduct = Office | Informatics | Book | Movie | Total
data Product = Product { value :: Double, tp :: TypeProduct} | Nothing 



