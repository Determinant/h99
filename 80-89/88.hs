import Data.List (partition, foldl')

data Graph a = Graph [a] [(a, a)] deriving (Show, Eq)

connComp :: Eq a => Graph a -> [[a]]

connComp (Graph v e) = fst $ foldl' 
                        (\(comps, vis) u -> if u `elem` vis then (comps, vis)
                                            else let (s, vis') = dfs u vis in 
                                                     (s:comps, vis'))
                        ([], []) v
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
