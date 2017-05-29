import Data.List (sortBy, insertBy)
import Data.Ord (comparing)

huffman :: [(Char, Int)] -> [(Char, String)]

huffman l = build $ map (\(a, b) -> ([(a, "")], b)) $ sortBy (comparing snd) l
    where build [x] = fst x
          build (x:y:xs) =
              build $ insertBy
                        (comparing snd)
                        ((add x '0') ++ (add y '1'), snd x + snd y) xs
                            where add e c = (map (\(a, b) -> (a, c:b)) $ fst e)
