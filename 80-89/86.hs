import Data.Ord (comparing)
import Data.List (foldl', sortBy)

data Graph a = Graph [a] [(a, a)] deriving (Show, Eq)
data Adjacency a = Adj [(a, [a])] deriving (Show, Eq)

graphToAdj :: Eq a => Graph a -> Adjacency a

graphToAdj (Graph [] _) = Adj []
graphToAdj (Graph (v:vs) e) = Adj ((v, e >>= pick):l)
    where pick (a, b)
            | a == v = [b]
            | b == v = [a]
            | otherwise = []
          Adj l = graphToAdj $ Graph vs e

kColor :: Eq a => Graph a -> [(a, Int)]

takeUntil _ [] = []
takeUntil p (x:xs) = x:if p x then [] else takeUntil p xs

kColor g = last $ takeUntil (\c' -> length c' == length vs') $ scanl (\c' x -> round c' x) [] [1..]
    where (Adj vs) = graphToAdj g
          vs' = sortBy (comparing (length . snd)) vs
          getc c u = filter ((== u) . fst) c
          ok c l cur = all (/= cur) [snd $ head col | v <- l, let col = getc c v, col /= []]
          round c cur = foldl' (\c' (u, adj) -> if (getc c' u == []) && ok c' adj cur then (u, cur):c' else c') c vs'
