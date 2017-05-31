data Tree a = Node a [Tree a] deriving (Eq, Show)

-- Haskell is type-safe, so there is no solution.
