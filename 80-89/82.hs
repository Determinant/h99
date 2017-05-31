import Data.List (partition)

cycle' :: Eq a => a -> [(a, a)] -> [[a]]

cycle' s edges = map reverse $ cycle'' s edges [s] False
    where
        cycle'' u edges path f
          | f && u == s = [path]
          | otherwise =
                do  let (vs, edges') = partition ((== u) . fst) edges
                    (_, v) <- vs
                    cycle'' v edges' (v:path) True
