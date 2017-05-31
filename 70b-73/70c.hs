import Data.List (foldl')

data Tree a = Node a [Tree a] deriving (Eq, Show)

nnodes :: Tree a -> Int

nnodes (Node _ xs) = 1 + (foldl' (+) 0 $ map nnodes xs)
