module Ex2 where


isPalindrome:: String -> Bool
isPalindrome xs = xs == reverse xs

isPalindromeList:: [String] -> [String]
isPalindromeList xs = filter isPalindrome xs


