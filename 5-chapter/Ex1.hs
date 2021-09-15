module Ex1 where

data TipoProduto = Escritorio | Informatica | Livro | Filme | Total
data Produto = Produto { valor :: Double, tp :: TipoProduto}