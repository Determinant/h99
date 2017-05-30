data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

symmetric :: (Eq a) => Tree a -> Bool
mirror :: (Eq a) => Tree a -> Tree a -> Bool

mirror Empty Empty = True
mirror (Branch _ a b) (Branch _ l r) = mirror a r && mirror b l
mirror _ _ = False

symmetric Empty = True
symmetric (Branch _ l r) = mirror l r
