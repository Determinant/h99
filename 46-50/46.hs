and', or', nand', nor', xor', impl', equ' :: Bool -> Bool -> Bool
not' :: Bool -> Bool

not' True = False
not' False = True

and' True True = True
and' _ _ = False

or' False False = False
or' _ _ = True

xor' True False = True
xor' False True = True
xor' _ _ = False

equ' a b = (not' a) `xor'` b
impl' a b = (not' a) `or'` b

nand' a = not' . and' a
nor' a = not' . or' a

table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ putStrLn [show a ++ " " ++ show b ++ " " ++ (show $ f a b)
                            | let bin = [True, False], a <- bin, b <- bin]
