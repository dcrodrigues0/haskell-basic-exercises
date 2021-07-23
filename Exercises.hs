module Exercises where

a = [1,11,121,1331,14641,161051,1771561]
b = [1,2,3,5,6,7,9,10,11,13,14,15,17,18,19,21,22,23,25,26,27,29,30,31,33,34,35,37,38,39]
c = ["AaBB","AbBB","AcBB","AdBB","AeBB","AfBB","AgBB"]
d = [5,8,11,17,20,26,29,32,38,41]
e = [1.0,0.5,0.25,0.125,0.0625,0.03125]
g = [2,4,8,10,12,16,18,22,24,28,30]
h = ['@','A','C','D','E','G','J','L']

isEven str          = (==) (mod(length(str)) 2) 0
reverseStr str      = reverse str
getOddSize strs     = [str | str <- strs, not (isEven str)]
getHead xs          = (last . reverse) xs
isPalindrom str     = (==) str (reverse str)
getSpecialTuple num = [(num*2,num*3),(num*4,num*5)]
