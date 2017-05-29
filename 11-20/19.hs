rotate :: [a] -> Int -> [a]

rotate l n = let (a, b) = rot l (if n < 0 then (n + length l) else n) in b ++ a
                 where rot l@(x:xs) n
                        | n > 0 = let (a, b) = rot xs (n - 1) in (x:a, b)
                        | otherwise = ([], l)
