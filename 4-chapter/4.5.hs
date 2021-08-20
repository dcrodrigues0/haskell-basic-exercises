module Ex5 where

doubleOfNumMultiplesOfFour:: [Int] -> [Int]
doubleOfNumMultiplesOfFour xs = [ x * 2 | x <- xs, mod 4 x /= 0]