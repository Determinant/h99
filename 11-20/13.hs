data ListItem a = Single a | Multiple Int a deriving Show
encodeDirect :: Eq a => [a] -> [ListItem a]

encodeDirect [] = []
encodeDirect [x] = [Single x]
encodeDirect (x:xs) = if x == h then n:ys else (Single x):(y:ys)
    where (y:ys) = encodeDirect xs
          helper (Single z) = (Multiple 2 z, z)
          helper (Multiple c z) = (Multiple (c + 1) z, z)
          (n, h) = helper y
