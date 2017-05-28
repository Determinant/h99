import Data.List
isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = foldl' (\flag (a, b) -> flag && a == b) True (zip l (reverse l))
