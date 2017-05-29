combinations :: Int -> [a] -> [([a], [a])]

combinations 0 xs = [([], xs)]
combinations _ [] = []
combinations k (x:xs) = [(x:t1, t2) | (t1, t2) <- combinations (k - 1) xs] ++
                        [(t1, x:t2) | (t1, t2) <- combinations k xs]

group :: [Int] -> [a] -> [[[a]]]

group _ [] = [[]]
group (n:ns) l = [a:r | (a, b) <- combinations n l, r <- group ns b]
