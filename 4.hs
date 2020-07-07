{- 4 Monoids are phantom Applicative functors -}
import Prelude hiding(Monoid(..), concat, any)

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
