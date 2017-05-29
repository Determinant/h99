isPrime :: Int -> Bool

isPrime 1 = False
isPrime x = and $ map (\y -> x `mod` y /= 0) $ fst $ span (\y -> y * y <= x) [2..]

goldbach :: Int -> (Int, Int)

goldbach x = head [(a, b) | a <- primes, let b = x - a, isPrime b]
    where primes = filterPrime[3..]
          filterPrime (p:xs) = p:filterPrime [x | x <- xs, x `mod` p /= 0]
