{- 7 Applicative functor, categorically -}

class Functor f => Monoidal f where
    unit :: f ()
    (*) :: f a -> f b -> f (a, b)

unit' :: Applicative f => f ()
unit' = pure ()

(>*<) :: Applicative f => f a -> f b -> f (a, b)
fa >*< fb = pure (,) <*> fa <*> fb

pure' :: Monoidal f => a -> f a
pure' x = fmap (\_ -> x) unit

(<*>^) :: Monoidal f => f (a -> b) -> f a -> f b
mf <*>^ mx = fmap (\(f, x) -> f x) (mf >*< mx)

(><) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
(f >< g) (x, y) = (f x, g y)

assoc :: (a, (b, c)) -> ((a, b), c)
assoc (a, (b, c)) = ((a, b), c)
