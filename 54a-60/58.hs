data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

cbalTree :: Int -> [Tree Char]

cbalTree 0 = [Empty]
cbalTree x
  | even n = [Branch 'x' lt rt | let st = cbalTree ln, lt <- st, rt <- st]
  | otherwise = let st1 = cbalTree ln
                    st2 = cbalTree (n - ln) in
                    [Branch 'x' lt rt | lt <- st1, rt <- st2] ++
                    [Branch 'x' lt rt | lt <- st2, rt <- st1]
    where ln = n `div` 2
          n = x - 1

symCbalTrees :: Int -> [Tree Char]

symCbalTrees x
  | even n = [Branch 'x' st $ reverseTree st | st <- cbalTree ln]
  | otherwise = []
  where ln = n `div` 2
        n = x - 1
        reverseTree Empty = Empty
        reverseTree (Branch x l r) = Branch x (reverseTree r) (reverseTree l)
