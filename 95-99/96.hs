import Data.Char (isLetter, isDigit)

identifier :: String -> Bool

identifier [] = False
identifier (c:xc) = isLetter c && loop xc
    where loop [] = True
          loop ('-':c:xc) = (isLetter c || isDigit c) && loop xc
          loop (c:xc) = (isLetter c || isDigit c) && loop xc
