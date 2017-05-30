data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

countLeaves :: Tree a -> Int

countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r) = countLeaves l + countLeaves r
