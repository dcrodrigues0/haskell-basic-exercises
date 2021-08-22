module Ex9 where

import Data.Function ((&))

countNegativeNumbers:: [Int] -> Int
countNegativeNumbers xs = xs
    & filter (<= 0)
    & foldl (+) 0

countPChar:: [Char] -> Int
countPChar xs = xs
    & 