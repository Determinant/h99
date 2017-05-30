data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch x l r) = x:internals l ++ internals r
