import Data.List (foldl')

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

symmetric :: (Eq a) => Tree a -> Bool
mirror :: (Eq a) => Tree a -> Tree a -> Bool

mirror Empty Empty = True
mirror (Branch _ a b) (Branch _ l r) = mirror a r && mirror b l
mirror _ _ = False

symmetric Empty = True
symmetric (Branch _ l r) = mirror l r

construct :: [Int] -> Tree Int
addNode :: Tree Int -> Int -> Tree Int

addNode Empty n = Branch n Empty Empty -- create node
addNode (Branch x l r) n
  | n <= x = Branch x (addNode l n) r
  | otherwise = Branch x l (addNode r n)

construct l = foldl' (\acc n -> addNode acc n) Empty l
