import Data.List (foldl')

myGCD :: Int -> Int -> Int


myGCD a 0 = abs a
myGCD a b = myGCD b (a `mod` b)

totient :: Int -> Int

totient x = foldl' (\acc y -> acc + if myGCD x y == 1 then 1 else 0) 0 [1..x]
