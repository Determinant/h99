myButLast [] = error "empty list"
myButLast [x] = error "list with single element"
myButLast (x:[y]) = x
myButLast (x:xs) = myButLast xs
