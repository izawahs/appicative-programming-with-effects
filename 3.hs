{- 3 Traversing data structures -}
import Prelude hiding (Applicative(..), Traversable(..))

infixl 4 <*>

class Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b


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
    dist     :: Applicative f => t (f a) -> f (t a)
    dist     = traverse id


newtype Id a = An { an :: a }

instance Applicative Id where
    pure = An
    An f <*> An x = An (f x)


data Tree a = Leaf | Node (Tree a) a (Tree a)

instance Traversable Tree where
    traverse f Leaf = pure Leaf
    traverse f (Node l x r) = pure Node <*> (traverse f l) <*> (f x) <*> (traverse f r) 
