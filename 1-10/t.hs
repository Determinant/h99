grp [] = []
grp (x:xs) = (x:(filter (==x) xs)):(grp $ filter (/=x) xs)
