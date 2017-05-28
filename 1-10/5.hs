myReverse l = f l []
    where
        f [] b = b
        f (x:xs) b = f xs (x:b)
