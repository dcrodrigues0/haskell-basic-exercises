module Ex2 where

-- Create temperature convert
data Temperature = Celsius | Kelvin | Farenheit

convertToCelsius degrees Kelvin    = (-) degrees 273.15
convertToCelsius degrees Farenheit = (/) ((-) degrees 32) 1.8000
convertToCelsius degrees _           = degrees

convertToKelvin degrees Celsius    = (+) degrees 273.15
convertToKelvin degrees Farenheit  = ((*)((+) degrees 459.67) (5/9))
convertToKelvin degrees _            = degrees

convertToFarenheit degrees Celsius = (+)((*) degrees 1.8) 32
convertToFarenheit degrees Kelvin  = ((-)((*) degrees (9/5)) 459.67)
convertToFarenheit degrees _         = degrees