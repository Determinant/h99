data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

layout :: Tree Char -> Tree (Char, (Int, Int))

-- layout': the second component of result is the distance of left edge to the
-- axis of root, and the thrid is the distance of right edge.

layout t = let (ret, ld, _) = layout' t x0 1
               x0 = 1 + maximum ld in ret
   where layout' :: Tree Char -> Int -> Int -> (Tree (Char, (Int, Int)), [Int], [Int])
         layout' Empty x y = (Empty, [], [])
         layout' (Branch v l r) x y = (Branch (v, (x, y)) lt rt, ld, rd)
              where (lt, lld, lrd) = layout' l (x - hsep) (y + 1)
                    (rt, rld, rrd) = layout' r (x + hsep) (y + 1)
                    hsep = 1 + (maximum $ 0:zipWith (+) lrd rld) `div` 2
                    ld = 0:map (+ hsep) lld
                    rd = 0:map (+ hsep) rrd
