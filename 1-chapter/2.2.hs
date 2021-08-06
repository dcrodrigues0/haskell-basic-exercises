module Ex2 where

--Verify if a string contains even number of chars

isEven str = (==) (mod(length(str)) 2) 0