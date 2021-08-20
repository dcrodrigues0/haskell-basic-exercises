module Ex4 where
import Data.Function

isPrimeNumber:: Int -> Int -> Bool
isPrimeNumber primeNumber i
    | primeNumber == 2    = True
    | primeNumber < 2     = False
    | mod primeNumber i   == 0 = False 
    | i * i > primeNumber = True
    | otherwise = isPrimeNumber primeNumber (i + 1)


filterPrimeNumber:: [Int] -> [Int]
filterPrimeNumber xs = xs
    & filter (> 1)
    & (\xs  -> [x | x <- xs, isPrimeNumber x 2])
