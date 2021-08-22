module Ex9 where

import Data.Function ((&))

data Dia = Getsuyoubi | Kayoubi | Suiyobi | Mokuyoubi | Kinyoubi | Doyoubi | Nichiyoubi deriving Show

countNegativeNumbers:: [Int] -> Int
countNegativeNumbers xs = xs
    & filter (<= 0)
    & foldl (+) 0

countPChar:: [Char] -> Int
countPChar xs = xs
    & filter (== 'P')
    & (\xs -> [ 1 | x <- xs])
    & foldl (+) 0

isDoyoubi:: Dia -> Bool
isDoyoubi Doyoubi = True
isDoyoubi _       = False

countDoyoubi:: [Dia] -> Int
countDoyoubi xs = xs
    & filter isDoyoubi
    & (\xs -> [1 | x <- xs])
    & foldl (+) 0

countDays:: [Dia] -> Int
countDays xs = xs
    & (\xs -> [1 | x <- xs])
    & foldl (+) 0

countDaysV2:: [Dia] -> Int -> Int
countDaysV2 xs count
    | count == length xs = count
    | otherwise = countDaysV2 xs $ count + 1