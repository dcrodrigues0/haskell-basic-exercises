module Ex13 where


data Meter = Meter { dimension :: Int, value :: Double, invalidMeter:: Double }

squareArea    (Meter dimension value _) = fromIntegral dimension * value

rectangleArea (Meter dimension value _) = fromIntegral dimension * value
cubeArea      (Meter dimension value _) = fromIntegral dimension * value
