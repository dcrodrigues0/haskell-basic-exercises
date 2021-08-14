module Ex1 where

meanDoubleList:: [Double] -> Double
meanDoubleList xs = foldl (+) 0 xs / fromIntegral (length xs)

