import Data.List (partition)

paths :: Eq a => a -> a -> [(a, a)] -> [[a]]

paths s t edges = map reverse $ paths' s edges [s]
    where
        paths' u edges path
            | u == t = [path]
            | otherwise =
                do  let (vs, edges') = partition ((== u) . fst) edges
                    (_, v) <- vs
                    paths' v edges' (v:path)
