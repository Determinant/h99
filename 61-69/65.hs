data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

layout :: Tree Char -> Tree (Char, (Int, Int))

layout t = draw t (1 + 2 ^ (dep - 1) - 2 ^ (dep - ldep)) 1 (2 ^ (dep - 2))
    where depth Empty = 0
          depth (Branch _ l r) = 1 + max (depth l) (depth r)
          ldepth Empty = 0
          ldepth (Branch _ l _) = 1 + ldepth l
          dep = depth t
          ldep = ldepth t
          draw Empty _ _ _ = Empty
          draw (Branch v l r) x y h = Branch (v, (x, y)) lt rt
                  where h' = h `div` 2
                        lt = draw l (x - h) (y + 1) h'
                        rt = draw r (x + h) (y + 1) h'
