fullWords :: Int -> String

fullWords x = if x < 10 then s else fullWords (x `quot` 10) ++ "-" ++ s
    where s = ["zero", "one", "two", "three", "four",
              "five", "six", "seven", "eight", "nine"]!!(x `mod` 10)
