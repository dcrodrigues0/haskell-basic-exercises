module Ex4 where

-- Create a function to return only vogals

isVowel x = 
  (==) x 'a' || 
  (==) x 'i' ||
  (==) x 'u' ||
  (==) x 'e' ||
  (==) x 'o' ||
  (==) x 'A' ||
  (==) x 'I' ||
  (==) x 'U' ||
  (==) x 'E' ||
  (==) x 'O'

getVowels xs = [ x | x <- xs, isVowel x ]