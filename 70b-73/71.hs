data Tree a = Node a [Tree a] deriving (Eq, Show)

ipl :: Tree a -> Int

ipl = ipl' 0
    where ipl' l (Node _ xs) = l + (sum $ map (ipl' (l + 1)) xs)
