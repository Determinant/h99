import System.Random
rndPermu :: [a] -> IO [a]

rndPermu l = d l (length l)
    where
        d l@(x:xs) n | n > 0 = do i <- getStdRandom $ randomR (0, (length l) - 1)
                                  r <- d (take i l ++ drop (i + 1) l) (n - 1)
                                  return (l!!i:r)
        d _ _ = return []
