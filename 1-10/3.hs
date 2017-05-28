elementAt [] _ = error "out of range"
elementAt (x:_) 1 = x
elementAt (_:xs) k
  | k < 1     = error "out of range" -- deal with the infinite list
  | otherwise = elementAt xs (k - 1)
