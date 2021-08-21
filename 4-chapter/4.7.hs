module Ex7 where

data Dia = Getsuyoubi | Kayoubi | Suiyobi | Mokuyoubi | Kinyoubi | Doyoubi | Nichiyoubi deriving Show

isDay:: Dia -> Bool
isDay Kayoubi = False
isDay _       = True

filterKayoubi:: [Dia] -> [Dia]
filterKayoubi xs = filter isDay xs
