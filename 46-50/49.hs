gray :: Int -> [String]

gray 0 = [""]
gray n = (map ('0':) last) ++ (map ('1':) $ reverse last) where last = gray (n - 1)
