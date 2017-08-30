{- EXERCISE: Implementing the Either Monad -}

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second b) = Second $ f b
  fmap f (First a) = First a 

instance Applicative (Sum a) where
  pure = Second
  (<*>) (First x) _  = First x
  (<*>) (Second f) (First x) = First x
  (<*>) (Second f) (Second x) = Second $ f x

instance Monad (Sum a) where
  return = pure
  (>>=) (First x) f = First x
  (>>=) (Second x) f = f x 
