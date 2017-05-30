data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

hbalTree :: a -> Int -> [Tree a]

hbalTree x 0 = [Empty]
hbalTree x 1 = [Branch x Empty Empty]
hbalTree x h =
    [Branch x lt rt
      | (h1, h2) <- [(h-2, h-1), (h-1, h-2), (h-1, h-1)]
      , lt <- hbalTree x h1
      , rt <- hbalTree x h2]
