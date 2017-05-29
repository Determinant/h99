slice :: [a] -> Int -> Int -> [a]

slice (x:xs) n m
    | n > 1 = slice xs (n - 1) (m - 1)
    | m > 0 = x:slice xs 0 (m - 1)
slice _ _ _ = []
