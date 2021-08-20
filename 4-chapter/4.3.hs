module Ex3 where

filterOdds:: [Int] -> [Int]
filterOdds xs = [ x | x <- xs, odd x]

filterEvens:: [Int] -> [Int]
filterEvens xs = [ x | x <- xs, even x]

filterOddsUsingFilter:: [Int] -> [Int]
filterOddsUsingFilter xs = filter odd xs

filterEvensUsingFilter:: [Int] -> [Int]
filterEvensUsingFilter xs = filter even xs

