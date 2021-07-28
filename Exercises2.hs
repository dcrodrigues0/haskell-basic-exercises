module Exercises2 where

-- 3.1 Crie o tipo Resposta com os values constructors Sim ou Não. Faça as funções, seguintes determinando seus tipos explicitamente.
data Answer = Yes | No

-- Recebe via parametro uma Resposta. Retorna 0 para Nao e 1 para Sim 
getAnswerNum Yes = 1
getAnswerNum No  = 0

-- Recebe via parametro uma lista de respostas e retorna 0s e 1s correspondentes aos constructores contidos na lista
listAnswers answers = [ getAnswerNum(answer) | answer <- answers ]

-- and' recebe duas perguntas como parametro e retorna a tabela verdade do and logico, usando sim como verdadeiro e nao como false
and' (Yes, Yes)  = True
and' (No, No)    = False
and' (Yes, No)   = False
and' (No, Yes)   = False

-- or' idem ao anterior porem com o ou logico
or'  (Yes, Yes)  = True
or'  (No, No)    = False
or'  (Yes, No)   = True
or'  (No, Yes)   = True

-- not' idem ao anterior porem com o not logico
not' (Yes, Yes)  = False
not' (No, No)    = True
not' (Yes, No)   = False
not' (No, Yes)   = False


-- Criar conversores de temperatura
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


-- Criar um jogo joquempo
data GameInput = Rock | Paper | Scisor deriving Show
data GameState = Winner | Loser | Tie deriving Show
newtype Player = Player {gameInput :: GameInput}

playRPSGame (Player Rock) Rock     = Tie
playRPSGame (Player Paper) Paper   = Tie
playRPSGame (Player Scisor) Scisor = Tie

playRPSGame (Player Rock) Scisor  = Winner
playRPSGame (Player Rock) Paper   = Loser

playRPSGame (Player Scisor) Rock  = Loser
playRPSGame (Player Scisor) Paper = Winner

playRPSGame (Player Paper) Rock   = Winner
playRPSGame (Player paper) Scisor = Loser

-- Faça uma funcao que retorne uma string com todas as vogais maiusculas e minusculas eliminadas de uma string passada por parametro

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

-- Faça uma função que converte os três tipos de unidades imperiais em metros

data ImperialUnity = Inch | Yard | Foot

convertToMeters inchs Inch = (/) inchs 39.37
convertToMeters yards Yard = (/) yards 1.094
convertToMeters foots Foot = (/) foots 3.281

convertToImperial meters Inch = (*) meters 39.37
convertToImperial meters Yard = (*) meters 1.094
convertToImperial meters Foot = (*) meters 3.281


-- Fazer a função checaFim que retorna o numero de dias que cada mes possui
data Month = January | February | March | April | May |June | July | August | September | October | November | Dezember

checkIfMonthIsFinished day January   = (==) day 31
checkIfMonthIsFinished day February  = (==) day 28
checkIfMonthIsFinished day March     = (==) day 31
checkIfMonthIsFinished day April     = (==) day 30
checkIfMonthIsFinished day May       = (==) day 31
checkIfMonthIsFinished day June      = (==) day 30
checkIfMonthIsFinished day July      = (==) day 31
checkIfMonthIsFinished day August    = (==) day 31
checkIfMonthIsFinished day September = (==) day 30
checkIfMonthIsFinished day October   = (==) day 31
checkIfMonthIsFinished day November  = (==) day 30
checkIfMonthIsFinished day Dezember  = (==) day 31

-- Fazer a função prox, que recebe um mês atual e retorna o proximo mes
nextMonth January   = February
nextMonth February  = March
nextMonth March     = April
nextMonth April     = May
nextMonth May       = June
nextMonth June      = July
nextMonth July      = August
nextMonth August    = September
nextMonth September = October
nextMonth October   = November
nextMonth November  = Dezember
nextMonth Dezember  = January

-- Fazer a Função estacao, que retorna a estação do ano de acordo com o mês e com hemisferio
data Hemisphere = North | South
data Season     = Summer | Autumn | Winter | Spring

getSeason January North   = Winter
getSeason February North  = Winter
getSeason March North     = Winter
getSeason April North     = Spring
getSeason May North       = Spring
getSeason June North      = Summer
getSeason July North      = Summer
getSeason August North    = Summer
getSeason September North = Summer
getSeason October North   = Autumn
getSeason November North  = Autumn
getSeason Dezember North  = Winter

getSeason January South   = Summer
getSeason February South  = Summer
getSeason March South     = Summer
getSeason April South     = Autumn
getSeason May South       = Autumn
getSeason June South      = Autumn
getSeason July South      = Winter
getSeason August South    = Winter
getSeason September South = Winter
getSeason October South   = Spring
getSeason November South  = Spring
getSeason Dezember South  = Summer

-- Faça uma função que receba uma String e retorne True se esta for um palíndromo; caso contrário False.
isPalindrome str = str == reverse str

--Faça uma função que elimine todos os números pares, todos os ímpares múltiplos de 7 e negativos de uma lista de 
--inteiros passada via parâmetro. Você deve retornar esta lista em ordem reversa em comparação a do paramêtro

identifyEvenAndSevenPlusPositiveNumber x = 
  odd x        &&
  mod x 7 /= 0 &&
  x > 0

getReversedAndFilteredList xs = reverse [ x | x <- xs, identifyEvenAndSevenPlusPositiveNumber x]

--Faça uma função que recebe três strings x, y e z como parâmetro. A função retorna uma tupla com três coordenadas
--contendo a ordem reversa em cada. A primeira coordenada deve conter string reversa do primeiro parâmetro, e assim por diante.

getReversedTupleOfStrings x y z = (reverse  x, reverse  y, reverse z)

--Faça uma função chamada revNum, que receba uma String s e um Int n. Esta deverá retornar as n primeiras letras em ordem 
--reversa e o restante em sua ordem normal. Ex: revNum 4 "FATEC" = "ETAFC"

revNum str num = reverse (take num str) ++ take (length str - num) (reverse str)

--Crie o tipo de dado Binario que pode ser Zero ou Um. Faça outro tipo de dado chamado Funcao que pode ser Soma2, Maior, Menor ou Mult2.
--Implemente a função aplicar que recebe uma Funcao e dois Binarios. Seu retorno consiste em executar a operação desejada. 
--Ex: aplicar Soma2 Um Um = Zero

data Binary   = Zero | Ichi
data Function = Sum2 | Maior | Menor | Mult2

apply Sum2 Zero Zero = Zero
apply Sum2 Zero Ichi = Ichi
apply Sum2 Ichi Zero = Ichi
apply Sum2 Ichi Ichi = Zero

apply Maior Zero Zero = Zero
apply Maior Zero Ichi = Ichi
apply Maior Ichi Zero = Ichi
apply Maior Ichi Ichi = Ichi

apply Menor Zero Zero = Zero
apply Menor Zero Ichi = Zero
apply Menor Ichi Zero = Zero
apply Menor Ichi Ichi = Ichi

apply Mult2 Zero Zero = Zero
apply Mult2 Zero Ichi = Zero
apply Mult2 Ichi Zero = Zero
apply Mult2 Ichi Ichi = Ichi

--Faça uma função chamada binList, usando List comprehension, que recebe uma lista de binários (exercicio anterior) e retorna outra 
--lista com elemento Somando Um e convertido para Int Ex: binList [Um Zero, Zero, Um, Zero] = [0,1,1,0,1]

binToInt Zero = 0
binToInt Ichi = 1
  
binList xs = [ binToInt (apply Sum2 x Ichi) | x <- xs]

--Faça um novo tipo chamado Metros, que possui um value constructor de mesmo nome, cujos parâmetros são: um Int que representa a dimensão,
--E um Double que representa o valor da medida e outro chamado metragemInvalida. Implemente as funções
--areaQuadrado, areaRet, aareaCubo

data Meter = Meter { dimension :: Int, value :: Double, invalidMeter:: Double }

squareArea    (Meter dimension value _) = fromIntegral dimension * value
rectangleArea (Meter dimension value _) = fromIntegral dimension * value
cubeArea      (Meter dimension value _) = fromIntegral dimension * value

data Valid = Hai String | Iie deriving Show 

isNameValid name = if null name then Iie else Hai name

--Faça o tipo Numero, que possui um value constructor Ok com um campo double e outro value constructor Erro com um campo String, Faça a 
--função dividir que divida dois números e, caso o segundo seja 0, emita um erro(usar pattern matching) ex: dividir (Numero 6) (Numero 5) -> Numero 1.2

data Number = Ok Double | Erro String deriving Show 

divisao (Ok x) (Ok 0)     = Erro "You can't do it"
divisao (Ok x) (Ok y)     = Ok (x/y)
divisao (Ok _) (Erro _)   = Erro "You can't do it"
divisao (Erro _) (Erro _) = Erro "You can't do it"
divisao (Erro _) (Ok _)   = Erro "You can't do it"

