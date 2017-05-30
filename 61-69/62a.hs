data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

atLevel :: Tree a -> Int -> [a]

atLevel Empty _ = []
atLevel (Branch x l r) n
  | n == 1 = [x]
  | n > 1 = atLevel l (n - 1) ++ atLevel r (n - 1)
  | otherwise = []
