import Data.List (foldr)

data Graph a = Graph [a] [(a, a)] deriving (Show, Eq)

bipartite :: Eq a => Graph a -> Bool

bipartite (Graph v e) =
    let t = takeWhile id $
                scanl (\acc x -> acc && (fst $ dfs x [] 0)) True v in
                    length t == 1 + length v
    where adjust u (a, b)
              | a == u = [b]
              | b == u = [a]
              | otherwise = []
          dfs u c cur
              | color /= [] = (if snd (head color) == cur then True else False, c)
              | otherwise = let vs = (e >>= (adjust u))
                                t = takeWhile fst $
                                        scanl (\(_, c) v -> dfs v c (1 - cur))
                                                (True, (u, cur):c) vs in
                                    (length t == 1 + length vs, snd $ last t)
                where color = filter ((== u) . fst) c
