import Data.List (intersperse)
data Tree a = Node a [Tree a] deriving (Eq, Show)

display :: Tree Char -> [Char]

display (Node v []) = [v]
display (Node v xs) = '(':v:' ':(concat $ intersperse " " $ map display xs) ++ ")"
