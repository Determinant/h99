data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

layout :: Tree Char -> Tree (Char, (Int, Int))

layout t = fst $ draw t 1 1
    where draw Empty x _ = (Empty, x)
          draw (Branch v l r) x y =
              (Branch (v, (x', y)) lt rt, x'')
                  where (lt, x') = draw l x (y + 1)
                        (rt, x'') = draw r (x' + 1) (y + 1)
