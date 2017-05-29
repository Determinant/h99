import System.Random
rndSelect :: [a] -> Int -> IO [a]

rndSelect l n 
    | n > 0 = do r <- rndSelect l (n - 1)
                 i <- getStdRandom $ randomR (0, (length l) - 1)
                 return (l!!i:r)
    | otherwise = return []
