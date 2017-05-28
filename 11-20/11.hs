pack :: Eq a => [a] -> [[a]]

pack [] = []
pack [x] = [[x]]
pack (x:xs) = if x == head y then (x:y):ys else [x]:(y:ys) where (y:ys) = pack xs

data ListItem a = Single a | Multiple Int a deriving Show
encodeModified :: Eq a => [a] -> [ListItem a]

encodeModified xs = map (\x -> let l = length x
                                   h = head x in
                           case l of
                             1 -> Single h
                             otherwise -> Multiple l h) $ pack xs
