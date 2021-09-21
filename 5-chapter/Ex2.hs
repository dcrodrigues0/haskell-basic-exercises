module Ex2 where
import Ex1

generalTotal:: [ProductEx] -> ProductEx
generalTotal xs = foldl (<>) mempty xs

