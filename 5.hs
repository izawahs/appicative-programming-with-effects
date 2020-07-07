{- 5 Applicative versus Monad? -}
{-# LANGUAGE TypeOperators #-}

miffy :: Monad m => m Bool -> m a -> m a -> m a
miffy mb mt me = do
    b <- mb
    if b then mt else me

iffy :: Applicative f => f Bool -> f a -> f a -> f a
iffy fb ft fe = pure cond <*> fb <*> ft <*> fe
    where
        cond b t e = if b then t else e

newtype (f . g) a = Comp { comp :: f (g a)}

instance Applicative (f . g) where
    pure x = Comp (pure (pure x))
    Comp fs <*> Comp xs = Comp (pure (<*>) <*> fs <*> xs)

data Except err a = OK a | Failed err

instance Monoid err => Applicative (Except err) where
    pure = OK
    OK f <*> OK x = OK (f x)
    OK f <*> Failed err = Failed err
    Failed err <*> OK x = Failed err
    Failed err1 <*> Failed err2 = Failed (err1 `oplus` err2)
