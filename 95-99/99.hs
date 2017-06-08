import Data.Function (on)
import Data.List (groupBy, delete)

toSites lines = sites id lines ++ sites transpose lines
    where sites trans ls = concatMap (
                                (filter ((> 1) . snd)) .
                                (map (\l -> (map fst l, length l))) .
                                (filter ((== '.') . snd . head)) .
                                (groupBy ((==) `on` snd))) $
                            trans ((zipWith (\r line -> zip [(r, c) | c <- [0..]] line) [0..]) ls)

transpose mat
    | head mat == [] = []
    | otherwise = (\(a, b) -> a:transpose b) $ unzip (map (\(x:xs) -> (x, xs)) mat)


solve :: String -> [[((Int, Int), Char)]]

solve str = solve' words sites []
    where solve' _ [] wrote = return wrote
          solve' words (s:ss) wrote =
            do w <- filter (\w -> length w == snd s) words
               let t = takeWhile snd $
                        scanl (\(wr, f) (c, p@(x, y)) ->
                            let t = filter ((== p). fst) wrote in
                                if t == [] then ((p, c):wr, f)
                                else if (snd . head) t == c then
                                        (wr, f) else ([], False))
                        (wrote, True)
                        (zip w $ fst s)
               if length t == snd s + 1 then
                  solve' (delete w words) ss (fst $ last t)
               else []


          (words, sitesin) = break (== "") $ lines str
          sites = toSites $ tail sitesin
