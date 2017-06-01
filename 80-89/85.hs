import Data.List (permutations, sort)

data Graph a = Graph [a] [(a, a)] deriving (Show, Eq)

iso :: Ord a => Graph a -> Graph a -> Bool

-- assumption: the graphs are bi-directional, without duplicate edges

iso (Graph v1 e1) (Graph v2 e2)
    | length v1 /= length v2 || length e1 /= length e2 = False
    | otherwise = let perm = permutations v2
                      t = takeWhile id $
                            scanl (\acc perm ->
                                let tab = zip v1 perm
                                    trans x = snd . head $ filter ((== x) . fst) tab in
                                    acc && (sort (map (\(u, v) -> (trans u, trans v)) e1') /= e2''))
                            True perm in length t /= length perm + 1
        where adjust e = e >>= (\(u, v) -> [(u, v), (v, u)])
              e1' = adjust e1
              e2'' = sort $ adjust e2

graphG1 = Graph [1,2,3,4,5,6,7,8]
                [(1,5),(1,6),(1,7),(2,5),(2,6),(2,8),(3,5),(3,7),(3,8),(4,6),(4,7),(4,8)]
graphH1 = Graph [1,2,3,4,5,6,7,8]
                [(1,2),(1,4),(1,5),(6,2),(6,5),(6,7),(8,4),(8,5),(8,7),(3,2),(3,4),(3,7)]
graphH1' = Graph [1,2,3,4,5,6,7,8]
                [(1,2),(1,4),(1,5),(6,2),(6,5),(6,7),(8,4),(8,5),(8,7),(3,2),(3,4),(3,6)]
