data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

stringToTree :: Monad m => [Char] -> m (Tree Char)

stringToTree "" = return (Empty)
stringToTree [x] = return (Branch x Empty Empty)
stringToTree x = fmap fst $ parse x
    where  parse (x:xs)
             | x == ',' || x == ')' = return (Empty, x:xs)
           parse (x:y:xs)
             | y == ',' || y == ')' = return (Branch x Empty Empty, y:xs)
             | y == '(' = do (lt, ',':xs') <- parse xs
                             (rt, ')':xs'') <- parse xs'
                             return (Branch x lt rt, xs'')
           parse _ = fail "parse error"

treeToPreorder :: Tree Char -> [Char]

treeToPreorder Empty = ""
treeToPreorder (Branch x l r) = x:treeToPreorder l ++ treeToPreorder r

treeToInorder :: Tree Char -> [Char]

treeToInorder Empty = ""
treeToInorder (Branch x l r) = treeToInorder l ++ [x] ++ treeToInorder r


preInTree :: Monad m => [Char] -> [Char] -> m (Tree Char)
preInTree [] [] = return Empty
preInTree (p:ps) is = do let (lis, _:ris) = break (== p) is
                             (lps, rps) = splitAt (length lis) ps
                         lt <- preInTree lps lis
                         rt <- preInTree rps ris
                         return $ Branch p lt rt
preInTree _ _ = fail "error"
