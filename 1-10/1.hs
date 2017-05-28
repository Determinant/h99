myLast [] = error "empty list"
myLast [x] = x
myLast (_:xs) = myLast xs
