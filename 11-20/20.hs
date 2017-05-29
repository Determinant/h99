removeAt :: Int -> [a] -> (Maybe a, [a])

removeAt k (x:xs)
  | k > 1 = let (a, b) = removeAt (k - 1) xs in (a, x:b)
  | k == 1 = (Just x, xs)
removeAt _ l = (Nothing, l)
