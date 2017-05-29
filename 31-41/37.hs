import Data.List (group, foldl')

primeFactors :: Int -> [Int]

primeFactors x = factor x primes
    where primes = filterPrime [2..]
          filterPrime (p:xs) = p:filterPrime [x | x <- xs, x `mod` p /= 0]
          factor x l@(p:xs)
            | p > x = []
            | x `mod` p == 0 = p:factor (x `div` p) l
            | otherwise = factor x xs

primeFactorsMult :: Int -> [(Int, Int)]

primeFactorsMult x = [(head l, length l) | l <- group $ primeFactors x]

phi :: Int -> Int

phi x = foldl' (\acc (p, m) -> acc * (p - 1) ^ m) 1 (primeFactorsMult x)
