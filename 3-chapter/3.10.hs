module Ex10 where

-- Create a function that receive a s String and a Int n, 
-- which should return the n first letters in order reverse and the rest in their normal order

revNum str num = reverse (take num str) ++ take (length str - num) (reverse str)