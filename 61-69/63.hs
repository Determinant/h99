import Data.List (foldl')

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

completeBinaryTree :: Int -> Tree Char
completeBinaryTree' :: Int -> Tree Char

-- This one is very slow because it simulates adding n nodes one by one to form
-- a complete binary tree. Although the time complexity is O(nlogn) for
-- generating a tree of size n, the use of floating point operations still
-- slows down the function a lot.

completeBinaryTree n = foldl' (\acc m -> addNode acc m) Empty [0..n-1]
    where addNode Empty _ = Branch 'x' Empty Empty
          addNode (Branch x l r) m
            | i >= ln = Branch x l $ addNode r (i - 1)
            | otherwise = Branch x (addNode l (m - ln)) r
            where lvl = floor $ logBase 2 $ fromIntegral $ m + 1
                  i = m - (2 ^ lvl - 1)
                  ln = 2 ^ (lvl - 1)

-- This one just grows a heap of size n, which is exactly a complete binary
-- tree, and the time complexity is O(n).
completeBinaryTree' n = gen 1
    where gen x
            | x > n = Empty
            | otherwise = Branch 'x' (gen $ x * 2) (gen $ x * 2 + 1)
