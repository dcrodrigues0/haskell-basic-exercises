module Ex5 where

-- Create a convert to Imperial Unitys

data ImperialUnity = Inch | Yard | Foot

convertToMeters inchs Inch = (/) inchs 39.37
convertToMeters yards Yard = (/) yards 1.094
convertToMeters foots Foot = (/) foots 3.281

convertToImperial meters Inch = (*) meters 39.37
convertToImperial meters Yard = (*) meters 1.094
convertToImperial meters Foot = (*) meters 3.281