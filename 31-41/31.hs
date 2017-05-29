isPrime :: Int -> Bool

isPrime 1 = False
isPrime x = and $ map (\y -> x `mod` y /= 0) $ fst $ span (\y -> y * y <= x) [2..]
