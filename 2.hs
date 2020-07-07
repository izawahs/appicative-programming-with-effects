{- 2 The Applicative class -}
import Prelude hiding (Applicative(..), sequence)

infixl 4 <*>

class Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
-- ^ These are Applicative laws:
-- Identity     pure id <*> u = u
-- Composition  pure (.) u <*> v <*> w = u <*> (v <*> w)
-- Homomorphism pure f <*> pure x = pure (f x)
-- Interchange  u <*> pure x = pure (\f -> f x) <*> x


(<$>) :: Applicative f => (a -> b) -> f a -> f b
f <$> u = pure f <*> u

instance Applicative ((->) env) where
    pure x = \γ -> x
    ef <*> ex = \γ -> (ef γ) (ex γ)


sequence :: [IO a] -> IO [a]
sequence []     = pure []
sequence (c:cs) = pure (:) <*> c <*> (sequence cs)

{-
eval :: Exp v -> Env v -> Int
eval (Var x) = fetch x
eval (Val i) = pure i
eval (Add p q) = pure (+) <*> eval p <*> eval q
-}

transpose :: [[a]] -> [[a]]
transpose [] = pure []
transpose (xs:xss) = pure (:) <*> xs <*> transpose'' xss
