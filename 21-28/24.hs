import System.Random
diffSelect :: Int -> Int -> IO [Int]

diffSelect n m = d [1..m] n
    where
        d l@(x:xs) n | n > 0 = do i <- getStdRandom $ randomR (0, (length l) - 1)
                                  r <- d (take i l ++ drop (i + 1) l) (n - 1)
                                  return (l!!i:r)
        d _ _ = return []
