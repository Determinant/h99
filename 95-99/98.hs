import Data.List (intersperse)

-- generate all possible patterns given row/col constraints
gen n []
    | n >= -1 = return []
    | otherwise = []
gen n (x:xs) = do p <- [0..n-x]
                  s <- gen (n - (p + x + 1)) xs
                  return ([p..p+x-1] ++ map (+ (p + x + 1)) s)

output sols = [do b <- sol
                  ('|':(intersperse '|' $
                            map (\x -> if x `elem` b then 'X' else '_') [0..7])
                    ++ "|\n") | sol <- sols]

-- eliminate the possible patterns according to the given information in
-- a column (b stands for 'X')
elim b w ps = filter (\p -> all (\x -> x `elem` p) b &&
                            all (\x -> x `notElem` p) w) ps

nonogram row col = output $ find 0 (map (gen 8) row) (map (gen 9) col)
                                   (replicate 8 []) (replicate 8 [])
    where find _ [] colps _ _
            | any (== []) colps = []
            | otherwise = return []
          find i (rs:rowps) colps cb cw
            | any (== []) colps = []
            | otherwise = do r <- rs
                             let cb' = [b' | (c, b) <- zip [0..7] cb,
                                             let b' = if c `elem` r then i:b else b]
                             let cw' = [w' | (c, w) <- zip [0..7] cw,
                                             let w' = if c `notElem` r then i:w else w]
                             sol <- find (i + 1) rowps
                                        (map ((uncurry . uncurry) elim)
                                                (zip (zip cb' cw') colps))
                                        cb' cw'
                             return (r:sol)

-- mapM_ putStr (nonogram [[3],[2,1],[3,2],[2,2],[6],[1,5],[6],[1],[2]] [[1,2],[3,1],[1,5],[7,1],[5],[3],[4],[3]])
