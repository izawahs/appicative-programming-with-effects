{- 6 Applicative functors and Arrows -}

class Arrow a where
    arr :: (b -> c) -> (a b c)
    (>>>) :: (a b c) -> (a c d) -> (a b d)
    first :: (a b c) -> (a (b, d) (c, d))

newtype EnvArrow  a env b = Env (a env b)

instance Arrow a => Applicative (EnvArrow a env) where
    pure x = Env (arr (const x))
    Env u <*> Env v = Env (u *** v >>> arr (\(f, x) -> f x))
        where u *** v = arr dup >>> first u >>> arr swap >>> first v >>> arr swap

dup a = (a, a)
swap (a, b) = (b, a)


newtype StaticArrow f a b c = Static (f (a b c))

instance (Applicative f, Arrow a) => Arrow (StaticArrow f a) where
    arr f = Static (pure (arr f))
    Static f >>> Static g = Static (pure (>>>) <*> f <*> g)
    first (Static f) = Static (pure first <*> f)
