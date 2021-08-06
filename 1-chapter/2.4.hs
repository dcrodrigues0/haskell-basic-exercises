module Ex4 where

--Get odd size from Even Strings
isEven str = (==) (mod(length(str)) 2) 0

getOddSize strs     = [str | str <- strs, not (isEven str)]