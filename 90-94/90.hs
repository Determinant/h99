import Data.Bits (shift, (.|.), (.&.), Bits)

queens :: Int -> [[Int]]

queens n = q 0 0 0 0
    where q :: Int -> Int -> Int -> Int -> [[Int]]
          q s l r d
              | s == n = return []
              | otherwise = do i <- [0..n-1]
                               let p = shift 1 i
                               if p.&.forb == 0 then
                                   map (i:) (q (s + 1)
                                     (shift (l.|.p) (-1))
                                     ((shift (r.|.p) 1).&.mask)
                                     (d.|.p))
                               else []
            where forb = l.|.r.|.d
                  mask = (shift 1 n) - 1
