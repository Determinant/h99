combinations :: Int -> [a] -> [[a]]

combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x:xs) = [x:t | t <- combinations (k - 1) xs] ++ combinations k xs
