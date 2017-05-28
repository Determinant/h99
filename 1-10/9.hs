pack :: Eq a => [a] -> [[a]]

pack [] = []
pack [x] = [[x]]
pack (x:xs) = if x == head y then (x:y):ys else [x]:(y:ys) where (y:ys) = pack xs
