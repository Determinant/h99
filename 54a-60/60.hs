data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

hbalTreeNodes :: a -> Int -> [Tree a]

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- The minimum height for a balanced tree
minHeight n = ceiling $ logBase 2 $ fromIntegral (n + 1)

-- The minimum node for a height
minNode h = fibs !! (h + 2) - 1

maxNode h = 2 ^ h - 1
maxHeight n = (length $ takeWhile (<= n + 1) fibs) - 3


hbalTreeNodes x n = [t | h' <- [minHeight n..maxHeight n], t <- build n h']
    where build _ 0 = [Empty]
          build _ 1 = [Branch x Empty Empty]
          build n h = let n' = n - 1 in
                  [Branch x lt rt | (h1, h2) <- [(h-2, h-1), (h-1, h-2), (h-1, h-1)]
                                  , n1 <- [max (n' - maxNode h2) $ minNode h1..
                                           min (n' - minNode h2) $ maxNode h1]
                                  , lt <- build n1 h1
                                  , rt <- build (n' - n1) h2]
