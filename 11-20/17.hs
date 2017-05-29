split :: [a] -> Int -> ([a], [a])

split [] _ = ([], [])
split l@(x:xs) n
  | n > 0 = let (ys, zs) = split xs (n - 1) in (x:ys, zs)
  | otherwise = ([], l)
