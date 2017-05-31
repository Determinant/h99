data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

ds2tree :: [Char] -> Tree Char

ds2tree l = fst $ build l
    where build ('.':xs) = (Empty, xs)
          build (x:xs) = (Branch x lt rt, xs'')
            where (lt, xs') = build xs
                  (rt, xs'') = build xs'

tree2ds :: Tree Char -> [Char]

tree2ds Empty = "."
tree2ds (Branch x l r) = x:tree2ds l ++ tree2ds r
