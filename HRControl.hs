module HRControl where

import Data.Function
data EmployType = Intern | Developer | Cordinator | Manager deriving Show
data Employ     = Employ {employType :: EmployType, name :: String} deriving Show 

getSalary (Employ Intern _)     = 1500.0
getSalary (Employ Developer _)  = 4000.0
getSalary (Employ Cordinator _) = 5000.0
getSalary (Employ Manager _)    = 6000.0

getProfile e = "{name: " ++ (name e) ++ "," ++ " employType: " ++ show (employType e) ++ "," ++ " salary: " ++ show (getSalary e) ++ "}"

promote (Employ Intern     e) = Employ Developer e
promote (Employ Developer  e) = Employ Cordinator e
promote (Employ Cordinator e) = Employ Manager e
promote (Employ _ e)          = Employ Manager e

initialHire = Employ Intern

meanSalary:: [Employ] -> Double 
meanSalary em = (foldl calc 0 em) / (fromIntegral  $ length em) 
    where calc salary employ = salary + getSalary employ

contractALotOfInterns ps = map initialHire ps

toPromote employ = employ
    & promote
    & getProfile

