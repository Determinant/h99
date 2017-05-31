data Tree a = Node a [Tree a] deriving (Eq, Show)

bottomUp :: Tree Char -> [Char]

bottomUp (Node v xs) = (concat $ map bottomUp xs) ++ [v]
