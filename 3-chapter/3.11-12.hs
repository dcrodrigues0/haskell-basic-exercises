module Ex11_12_13 where


data Binary   = Zero | One
data Function = Sum2 | Maior | Menor | Mult2

apply Sum2 Zero Zero = Zero
apply Sum2 Zero One = One
apply Sum2 One Zero = One
apply Sum2 One One = Zero

apply Maior Zero Zero = Zero
apply Maior Zero One = One
apply Maior One Zero = One
apply Maior One One = One

apply Menor Zero Zero = Zero
apply Menor Zero One = Zero
apply Menor One Zero = Zero
apply Menor One One = One

apply Mult2 Zero Zero = Zero
apply Mult2 Zero One = Zero
apply Mult2 One Zero = Zero
apply Mult2 One One = One

--Faça uma função chamada binList, usando List comprehension, que recebe uma lista de binários (exercicio anterior) e retorna outra 
--lista com elemento Somando Um e convertido para Int Ex: binList [Um Zero, Zero, Um, Zero] = [0,1,1,0,1]

binToInt Zero = 0
binToInt One = 1
  
binList xs = [ binToInt (apply Sum2 x One) | x <- xs]