module Ex19_20_21 where

data Exchange = Euro | Real | Dollar deriving Show
data Currency  = Currency { val :: Double, cur :: Exchange} deriving Show

convertToEuro (Currency val Real)   = Currency (val * 6.18)  Euro
convertToEuro (Currency val Dollar) = Currency (0.84 / val)  Euro
convertToEuro (Currency val _)      = Currency val Euro

convertToDollar (Currency val Real) = Currency (0.19 / val)  Dollar
convertToDollar (Currency val Euro) = Currency (val * 1.19)  Dollar
convertToDollar (Currency val _)    = Currency val Dollar

convertToReal (Currency val Euro)    = Currency (val * 6.18)  Real
convertToReal (Currency val Dollar)  = Currency (val * 5.21)  Real
convertToReal (Currency val _)       = Currency val Real

convertAllToReal xs = [convertToReal x | x <- xs]

convertToList xs = [x | x <- xs];

getVal (Currency val _)   = val

maxMoeda moedas = maximum [ getVal moeda | moeda <- moedas]