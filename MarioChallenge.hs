

verifyPredicate:: [Char] -> Int -> [Char] -> Bool
verifyPredicate xs count stack
    | xs == [] = False
    | xs !! count == '(' = verifyPredicate xs (count + 1) $ stack ++ ['(']
    | otherwise = verifyPredicate xs (count + 1) $ init stack
    
    
