module Ex6 where

func:: (String -> String) -> String -> String
func f s = reverse s ++ f s