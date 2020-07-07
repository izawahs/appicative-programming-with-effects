import Prelude hiding (sequence, repeat)

sequence :: [IO a] -> IO [a]
sequence []     = return []
sequence (c:cs) = do
    x  <- c
    xs <- sequence cs
    return (x:xs)

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf mx = do
    f <- mf
    x <- mx
    return (f x)

sequence' :: [IO a] -> IO [a]
sequence' []     = return []
sequence' (c:cs) = return (:) `ap` c `ap` sequence cs

transpose :: [[a]] -> [[a]]
transpose [] = repeat []
transpose (xs:xss) = zipWith (:) xs (transpose xss)

repeat :: a -> [a]
repeat x = x : repeat x

zapp :: [a -> b] -> [a] -> [b]
zapp (f:fs) (x:xs) = f x : zapp fs xs
zapp _      _      = []

{-
zipWithN :: (a1 -> ... -> an -> b) -> [a1] -> ... [an] -> [b]
zipWithN f xs1 ... xs2 = repeat f `zapp` xs1 `zapp` ... `zapp` xsn
-}

transpose' :: [[a]] -> [[a]]
transpose' []       = repeat []
transpose' (xs:xss) = repeat (:) `zapp` xs `zapp` transpose xss


data Exp v = Var v
           | Val Int
           | Add (Exp v) (Exp v)

{-
eval :: Exp v -> Env v -> Int
eval (Var x)   γ = fetch x γ
eval (Val i)   γ = i
eval (Add p q) γ = eval q γ + eval q γ

eval' :: Exp v -> Env v -> Int
eval' (Var x) = fetch x
eval' (Val i) = k i
eval' (Add p q) = k (+) `s` eval p `s` eval q
-}

k :: a -> env -> a
k x γ = x

s :: (env -> a -> b) -> (env -> a) -> (env -> b)
s ef es γ = (ef γ) (es γ)
