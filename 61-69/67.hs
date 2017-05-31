data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

treeToString :: Tree Char -> [Char]

treeToString Empty = ""
treeToString (Branch x Empty Empty) = [x]
treeToString (Branch x l r) = x:'(':(treeToString l) ++ "," ++ (treeToString r) ++ ")"

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


cbtFromList :: [a] -> Tree a

cbtFromList xs = let (t, xss) = cbt (xs:xss) in t
                     where cbt ((x:xs):xss) = (Branch x lt rt, xs:xss'')
                               where (lt, xss') = cbt xss
                                     (rt, xss'') = cbt xss'
                           cbt _ = (Empty, [])
