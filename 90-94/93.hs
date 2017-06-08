puzzle :: [Integer] -> [String]

puzzle l = do i <- [1..length l-1]
              let (subl, subr) = splitAt i l
              (sl, vl, _) <- gen subl
              (sr, vr, _) <- gen subr
              if vl == vr then
                  return (sl ++ " = " ++ sr)
              else []

gen :: [Integer] -> [(String, Rational, String)]
gen (x:[]) = return (show x, fromInteger x, "_")

gen l = do i <- [1..length l-1]
           let (subl, subr) = splitAt i l
           (sl, vl, opsl) <- gen subl
           (sr, vr, opsr) <- gen subr
           (ops, op) <- [("+", (+)), ("-", (-)), ("*", (*)), ("/", (/))]
           if (ops == "/" && vr == 0) ||
               (ops == "+" && (opsr == "+" || opsr == "-")) ||
               (ops == "*" && (opsr == "*" || opsr == "/")) then []
           else
              return ((if opsl /= "_" &&
                          (ops == "*" || ops == "/") &&
                          (opsl == "+" || opsl == "-") then
                           "(" ++ sl ++ ")"
                      else sl)
                      ++ " " ++ ops ++ " " ++
                      (if opsr /= "_" &&
                           ((ops == "-" && opsr /= "*" && opsr /= "/") ||
                           (ops == "*" && (opsr == "+" || opsr == "-")) ||
                           ops == "/") then
                           "(" ++ sr ++ ")"
                      else sr), op vl vr, ops)
