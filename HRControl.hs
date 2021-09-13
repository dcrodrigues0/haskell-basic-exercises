module Project where

import Data.Function
import Data.Monoid

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

data Project = Project {projectName :: String, budget :: Double, involved :: [Int]} deriving Show

class ToJSON a where
    toJSON :: a -> String

instance ToJSON Employ where
    toJSON e = "{name: \"" ++ (name e) ++
                "\", employType: \"" ++ show (employType e) ++
                "\", salary: " ++ show (getSalary e) ++ "}"

instance ToJSON Project where
    toJSON p = "{name: \"" ++ (projectName p) ++
                "\", budget: \"" ++ show (budget p) ++
                "\", involved: " ++ show (involved p) ++ "}"

instance Semigroup Monoid Project where
    Project p1 <> Project p2 = Project 

instance Monoid Project where
    mempty = Project "" 0 []
    mappend (Project name1 budget1 inv1) (Project name2 budget2 inv2) 
        = Project (name1 ++ ", " ++ name2) (budget1 + budget2) (inv1 ++ inv2)
