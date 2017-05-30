data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

-- Haskell is type-safe, so there is no solution.
