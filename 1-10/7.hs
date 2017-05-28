data NestedList a = Elem a | List [NestedList a]

flatten (Elem x) = [x]
flatten (List xs) = concat $ map flatten xs
