module Ex17_18 where

data Cripto = Message String | Encrypted String | Error deriving Show

encrypt (Message xs) = Message [ succ x | x <- xs] 
encrypt (Encrypted xs)  = Error
encrypt _             = Error

decrypt (Encrypted xs)  = Encrypted [ pred x | x <- xs]
decrypt (Message xs) = Error
decrypt _             = Error

encryptAll xs = [encrypt x | x <- xs]
