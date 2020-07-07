-- import Prelude hiding (Applicative(..), Traversable(..), Monoid(..), (<$>), sequence, repeat, concat, any)

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

eval :: Exp v -> Env v -> Int
eval (Var x)   γ = fetch x γ
eval (Val i)   γ = i
eval (Add p q) γ = eval q γ + eval q γ

eval' :: Exp v -> Env v -> Int
eval' (Var x) = fetch x
eval' (Val i) = k i
eval' (Add p q) = k (+) `s` eval p `s` eval q

k :: a -> env -> a
k x γ = x

s :: (env -> a -> b) -> (env -> a) -> (env -> b)
s ef es γ = (ef γ) (es γ)


infixl 4 <*>

class Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> b

-- ^ These are Applicative laws:
-- Identity     pure id <*> u = u
-- Composition  pure (.) u <*> v <*> w = u <*> (v <*> w)
-- Homomorphism pure f <*> pure x = pure (f x)
-- Interchange  u <*> pure x = pure (\f -> f x) <*> x


(<$>) :: Applicative f => (a -> b) -> f a -> f b
f <$> u = pure f <*> ua

instance Applicative ((->) env) where
    pure x = \γ -> x
    ef <*> ex = \γ -> (ef γ) (ex γ)

-- 'sequence'' in Applicative style
sequence'' :: [IO a] -> IO [a]
sequence'' []     = pure []
sequence'' (c:cs) = pure (:) <*> sequence cs

{-
eval :: Exp v -> Env v -> Int
eval (Var x) = fetch x
eval (Val i) = pure i
eval (Add p q) = pure (+) <*> eval p <*> eval q
-}

transpose'' :: [[a]] -> [[a]]
transpose'' [] = pure []
transpose'' (xs:xss) = pure (:) <*> xs <*> transpose'' xss


{- 3 Traversing data structures -}

-- 'applicative distributor'
dist' :: Applicative f => [f a] -> f [a]
dist'     [] = pure []
dist' (v:vs) = pure (:) <*> v <*> dist' vs

flakyMap :: (a -> Maybe b) -> [a] -> Maybe [b]
flakyMap f ss = dist (fmap f ss)

traverse' :: Applicative f => (a -> f b) -> [a] -> f [b]
traverse' f [] = pure []
traverse' f (x:xs) = pure (:) <*> f x <*> traverse' f xs


class Traversable t  where
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    dist     :: Applicative f => t (f a) -> f (t b)
    dist     = traverse id


newtype Id a = An { an :: a }

instance Applicative Id where
    pure = An
    An f <*> An x = An (f x)


data Tree a = Leaf | Node (Tree a) a (Tree a)

instance Traversable Tree where
    traverse f Leaf = pure Leaf
    traverse f (Node l x r) = pure Node <*> (traverse f l) <*> (f x) <*> (traverse f r) 


{- 4 Monoids are phantom Applicative functors -}

class Monoid o where
    ozero :: o
    oplus :: o -> o -> o

newtype Accy o a = Acc { acc :: o}

instance Monoid o => Applicative (Accy o) where
    pure _ = Acc ozero
    Acc o1 <*> Acc o2 = Acc (o1 `oplus` o2)

accumlate :: (Traversable t, Monoid o) => (a -> o) -> t a -> o
accumlate f = acc . traverse (Acc . f)

reduce :: (Traversable t, Monoid o) => t o -> o
reduce = accumlate id

flatten :: Tree a -> [a]
flatten = accumlate (:[])

concat :: [[a]] -> [a]
concat = reduce

newtype Mighty = Might { might :: Bool }

instance Monoid Mighty where
    ozero = Might False
    Might x `oplus` Might y = Might (x || y)

any :: Traversable t => (a -> Bool) -> t a -> Bool
any p = might . accumlate (Might . p)


{- 5 Applicative versus Monad? -}

miffy :: Monad m => m Bool -> m a -> m a -> m a
miffy mb mt be = do
    b <- mb
    if b then mt else me

iffy :: Applicative f => f Bool -> f a -> f a -> f a
iffy fb ft fe = pure cond <*> fb <*> ft <*> fe
    where
        cond b t e = if b then t else e

newtype (f . g) a = Comp { comp :: f (g a)}

instance Applicative Comp where
    pure x = Comp (pure (pure x))
    Comp fs <*> Comp xs = Comp ((<*>) <*> fs <*> xs)

data Except err a = OK a | Failed err

instance Monoid err => Applicative (Except err) where
    pure = OK
    OK f <*> OK x = OK (f x)
    OK f <*> Failed err = Failed err
    Failed err <*> OK x = Failed err
    Failed err1 <*> Failed err2 = Failed (err1 `oplus` err2)

class Arrow (~>) where
    arr :: (a -> b) -> (a ~> b)
    (>>>) :: (a ~> b) -> (b ~> c) -> (c ~> d)
    first :: (a ~> b) -> ((a, c) ~> (b, c))

newtype EnvArrow (~>) env a = Env (env ~> a)

instance Arrow (~>) => Applicative (EnvArrow (~>) env) Data where
    pure x = Env (arr (const x))
    Env u <*> Env v = Env (u *** v >>> arr (\(f, x) -> f x))
        where u *** v = arr dup >>> first u >>> arr swap >>> first v >>> arr swap

dup a = (a, a)
swap (a, b) = (b, a)


newtype StaticArrow f (~>) a b = Static (f (a ~> b))

instance (Applicative f, Arrow (~>)) => Arrow (StaticArrow f (~>)) where
    arr f = Static (pure (arr f))
    Static f >>> Static g = Static ((>>>) f g)
    first (Static f) = Static (pure (first f))


class Functor f => Monoidal f where
    unit :: f ()
    (*) :: f a -> f b -> f (a, b)

unit :: Applicative f => f ()
unit = pure ()

(>*<) :: Applicative f => f a -> f b -> f (a, b)
fa >*< fb = pure (,) <*> fa <*> fb

pure :: Monoidal f => a -> f a
pure x = fmap (\_ -> x) unit

(<*>) :: Monoidal f => f (a -> b) -> f a -> f b
mf <*> mx = fmap (\(f, x) -> f x) (mf >*< mx)

(><) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
(f >< g) (x, y) = (f x, g y)

assoc :: (a, (b, c)) -> ((a, b), c)
assoc (a, (b, c)) = ((a, b), c)

