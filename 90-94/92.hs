import Data.List (sortBy)
import Data.Ord (comparing)

vonKoch :: [(Int, Int)] -> [[(Int, Int)]]

vonKoch edges = koch (reverse adj) [] [] []
    where koch [] _ _ vplan = return vplan
          koch ((v, us):vs) eused vused vplan =
            do (vn, eused') <-  [(x, eused')
                                | x <- [1..maxvn],
                                  x `notElem` vused,
                                  let en = map (abs . (x-)) (map (findn vplan) us),
                                  let (eused', f) = foldl (\(l, f) d ->
                                        (d:l, f && d `notElem` l
                                                && 1 <= d && d <= maxen))
                                        (eused, True) en, f]
               koch vs eused' (vn:vused) ((v, vn):vplan)
          maxen = length edges
          maxvn = maxen + 1
          deg x = length $ filter (\(a, b) -> a == x || b == x) edges
          genAdj [] = []
          genAdj (v:vs) = (v, [u | u <- vs, (v, u) `elem` edges || (u, v) `elem` edges]):genAdj vs
          findn vplan u = snd $ head $ filter ((== u) . fst) vplan
          adj = genAdj $ sortBy (comparing deg) [1..maxvn]
