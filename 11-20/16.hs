dropEvery :: [a] -> Int -> [a]

dropEvery l n = d l 1
    where d [] i = []
          d (x:xs) i
            | i == n = d xs 1
            | otherwise = x:d xs (i + 1)
