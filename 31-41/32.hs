myGCD :: Int -> Int -> Int


myGCD a 0 = abs a
myGCD a b = myGCD b (a `mod` b)
