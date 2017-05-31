data Tree a = Node a [Tree a] deriving (Eq, Show)

stringToTree :: [Char] -> Tree Char

stringToTree l = fst $ build l
    where build (x:xs) = (Node x chd'', xs'')
            where loop ('^':xs) = ([], xs)
                  loop xs =  (chd':c, x)
                      where (chd', xs') = build xs
                            (c, x) = loop xs'
                  (chd'', xs'') = loop xs


treeToString :: Tree Char -> [Char]

treeToString (Node v xs) = v:(concat $ map treeToString xs) ++ "^"
