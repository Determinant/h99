primesR :: Int -> Int -> [Int]

primesR l r = takeWhile (<= r) $ dropWhile (< l) primes
    where primes = filterPrime[2..]
          filterPrime (p:xs) = p:filterPrime [x | x <- xs, x `mod` p /= 0]
