insertAt :: a -> [a] -> Int -> [a]

insertAt e (x:xs) n | n > 1 = x:insertAt e xs (n - 1)
insertAt e l 1 = e:l
insertAt _ l _ = l
