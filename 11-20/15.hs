repli :: [a] -> Int -> [a]

repli l n = rep l n
    where rep [] i = []
          rep (x:xs) i
            | i > 0 = x:(rep (x:xs) (i - 1))
            | otherwise = rep xs n
