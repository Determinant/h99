compress :: Eq a => [a] -> [a]
compress (x:y:xs) = if x == y then compress(y:xs) else x:compress(y:xs)
compress [] = []
compress [x] = [x]
