import Data.List (permutations, sort)

iso vs e1 e2 =
        let perm = permutations vs
            t = takeWhile id $ scanl (\acc perm ->
                                let tab = zip vs perm
                                    trans x = snd . head $ filter ((== x) . fst) tab in
                                    acc && (sort (map (\(u, v) -> (trans u, trans v)) e1') /= e2''))
                            True perm in length t /= length perm + 1
        where adjust e = e >>= (\(u, v) -> [(u, v), (v, u)])
              e1' = adjust e1
              e2'' = sort $ adjust e2

select _ 0 = [[]]
select [] _ = []
select (x:xs) k = (map (x:) $ select xs (k - 1)) ++ select xs k

regular :: Int -> Int -> [[(Int, Int)]]
regular n k = elim $ reg [] 0 []
    where elim [] = []
          elim (p:ps)
            | length ps + 1 == length (takeWhile id $
                                      scanl (\f p' -> f && not (iso' p p'))
                                            True ps) = p:elim ps
            | otherwise = elim ps
            where iso' = iso [0..n-1]
          reg e i plan
            | i == n = [plan]
            | otherwise = do vs <- select rv (k - k')
                             let plan' = [(i, v) | v <- vs] ++ plan
                             reg (vs ++ e) (i + 1) plan'
            where (_, k'):r =  map (\v -> (v, length $ filter (== v) e)) [i..n-1]
                  rv = map fst $ filter ((<k) . snd) r
