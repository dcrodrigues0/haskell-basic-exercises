module Ex16 where


data Number = Ok Double | Error String deriving Show 

divide (Ok x) (Ok 0)     = Error "You can't do it"
divide (Ok x) (Ok y)     = Ok (x/y)
divide (Ok _) (Error _)   = Error "You can't do it"
divide (Error _) (Error _) = Error "You can't do it"
divide (Error _) (Ok _)   = Error "You can't do it"