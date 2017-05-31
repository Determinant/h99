import Data.List (sort)

data Graph a = Graph [a] [(a, a)]
               deriving (Show, Eq)
                
data Adjacency a = Adj [(a, [a])]
           deriving (Show, Eq)
            
data Friendly a = Edge [(a, a)]
          deriving (Show, Eq)

graphToAdj :: Eq a => Graph a -> Adjacency a

graphToAdj (Graph [] _) = Adj []
graphToAdj (Graph (v:vs) e) = Adj ((v, e >>= pick):l)
    where pick (a, b)
            | a == v = [b]
            | b == v = [a]
            | otherwise = []
          Adj l = graphToAdj $ Graph vs e

adjToGraph :: Eq a => Adjacency a -> Graph a

adjToGraph (Adj []) = Graph [] []
adjToGraph (Adj ((u, e):ps)) = Graph (u:us)
                                     ((map (\v -> (u, v)) e) ++
                                      (filter (\(a, b) -> a /= u && b /= u) es))
    where (Graph us es) = adjToGraph $ Adj ps

graphToFri :: Eq a => Graph a -> Friendly a

graphToFri (Graph vs e) =
    Edge (e ++ let g = filter (\v -> all (\(a, b) -> v /= a && v /= b) e) vs in
                   zip g g)

friToGraph :: Ord a => Friendly a -> Graph a

friToGraph (Edge es) = Graph vs' (filter (\(a, b) -> a /= b) es')
    where unique [] = []
          unique [x] = [x]
          unique (x:l@(y:xs))
            | x == y = unique l
            | otherwise = x:unique l
          es' = unique . sort $ map (\(a, b) -> if a < b then (a, b) else (b, a)) es
          vs' = unique . sort $ es' >>= (\(a, b) -> [a, b])

adjToFri :: Eq a => Adjacency a -> Friendly a
friToAdj :: Ord a => Friendly a -> Adjacency a

adjToFri = graphToFri . adjToGraph
friToAdj = graphToAdj . friToGraph
