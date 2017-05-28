data ListItem a = Single a | Multiple Int a deriving Show
decodeModified :: [ListItem a] -> [a]

decodeModified [] = []
decodeModified (Single x:xs) = x:(decodeModified xs)
decodeModified (Multiple l x:xs) = replicate l x ++ decodeModified xs
