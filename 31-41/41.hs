isPrime :: Int -> Bool

isPrime 1 = False
isPrime x = and $ map (\y -> x `mod` y /= 0) $ fst $ span (\y -> y * y <= x) [2..]

goldbach :: Int -> (Int, Int)

goldbach x = head [(a, b) | a <- primes, let b = x - a, isPrime b]
    where primes = filterPrime[3..]
          filterPrime (p:xs) = p:filterPrime [x | x <- xs, x `mod` p /= 0]

goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList' :: Int -> Int -> Int -> [(Int, Int)]

goldbachList l r = map goldbach $ filter even $ dropWhile (< 4) [l..r]
goldbachList' l r i = filter (\(a, b) -> a > i && b > i) $ goldbachList l r
