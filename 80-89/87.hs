import Data.List (partition, foldl')

data Graph a = Graph [a] [(a, a)] deriving (Show, Eq)

depthFirst :: Eq a => Graph a -> a -> [a]

depthFirst (Graph v e) s = fst $ dfs s []
    where adjust u (a, b)
              | a == u = [b]
              | b == u = [a]
              | otherwise = []
          dfs u vis
              | u `elem` vis = ([], vis)
              | otherwise = let vs = (e >>= (adjust u)) in
                                foldl' (\(s, vis') v ->
                                        let (s', vis'') = dfs v vis' in
                                            (s ++ s', vis''))
                                       ([u], u:vis) vs
