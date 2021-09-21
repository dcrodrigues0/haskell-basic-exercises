module Ex3 where
  
newtype Min = Min Double deriving (Ord, Eq, Show)

instance Semigroup Min where
  (<>) (Min a) (Min b) = Min $ min a b

instance Monoid Min where
  mempty = Min 0
