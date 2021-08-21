module Ex8 where

import Data.Function

data Exchange = Real | Dollar deriving Show
data Currency  = Currency { val :: Double, cur :: Exchange} deriving Show

convertRealToDolar:: Currency -> Currency
convertRealToDolar (Currency x Real) = Currency (x / 5) Dollar
convertRealToDolar (Currency x _) = Currency x Dollar

convertDollarToReal :: Currency -> Currency
convertDollarToReal (Currency x Dollar) = Currency (x * 5) Real
convertDollarToReal (Currency x _) = Currency x Real

convertCurrencys:: Exchange -> [Currency] -> [Currency]
convertCurrencys Real   xs = map convertDollarToReal xs
convertCurrencys Dollar xs = map convertRealToDolar xs

isDollar :: Currency -> Bool
isDollar (Currency _ Dollar) = True 
isDollar (Currency _ _)      = False

filterDollars:: [Currency] -> [Currency]
filterDollars xs = filter isDollar xs

getVal:: Currency -> Double
getVal (Currency x _) = x

sumDollars:: [Currency] -> Double
sumDollars xs = xs
    & filterDollars
    & (\xs -> foldl (+) 0 [getVal x | x <- xs])


howManyDollarsInAList:: [Currency] -> Int -> Int
howManyDollarsInAList xs count
    | count == length xs = count
    | otherwise = howManyDollarsInAList xs $ count + 1
