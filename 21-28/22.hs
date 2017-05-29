range :: Int -> Int -> [Int]

range l r
    | l <= r = l:range (l + 1) r
    | otherwise = []
