module Ex14 where

data Valid = Yes String | No deriving Show 

isNameValid name = if null name then No else Yes name