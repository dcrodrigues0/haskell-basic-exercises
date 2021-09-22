module Ex5 where

data Parity = Even | Odd deriving Show

class OddEven a where
  decide:: a -> Parity

instance OddEven Int where
  decide x 
    | even x = Even
    | odd x  = Odd

instance OddEven [a] where
  decide x
    | even $ length x = Even
    | odd $ length x = Odd