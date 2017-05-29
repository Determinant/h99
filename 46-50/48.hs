import Control.Monad

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

tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = mapM_ putStrLn [(unwords $ map show args) ++  " " ++ (show $ f args)
                                | args <- replicateM n [True, False]]

infixl 4 `or'`
infixl 5 `xor'`
infixl 6 `and'`
infixl 7 `equ'`
--infixl 3 `equ'`
