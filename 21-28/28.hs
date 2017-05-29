import Data.List (sortBy, foldl', groupBy)
import Data.Ord (comparing)

lsort :: [[a]] -> [[a]]

lsort = sortBy (comparing length)

lfsort :: [[a]] -> [[a]]

lfsort l =
    [a | (a, b) <- sortBy (\x y -> (snd x) `compare` (snd y)) zipped]
    where larr = map length l
          count l e = foldl' (\acc x -> acc + (if x == e then 1 else 0)) 0 l
          zipped = zip l (map (count larr) larr)

lfsort2 = concat . lsort . groupBy (\x y -> length x == length y) . lsort
