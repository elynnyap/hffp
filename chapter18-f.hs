import Control.Monad(join, liftM, liftM2, return, ap)

{- CHAPTER EXERCISES -}

-- 1 
data Nope a = 
  NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap f NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  (<*>) _ _ = NopeDotJpg
  pure _ = NopeDotJpg

instance Monad Nope where
  (>>=) _ _ = NopeDotJpg

-- 2
data PhhhbbtttEither b a = 
    Left' a
  | Right' b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Left' a) = Left' $ f a
  fmap f (Right' b) = Right' b

instance Applicative (PhhhbbtttEither b) where
  pure = Left'
  (<*>) (Right' f) _ = Right' f
  (<*>) (Left' f) (Left' x) = Left' $ f x
  (<*>) (Left' f) (Right' x) = Right' x

instance Monad (PhhhbbtttEither b) where
  return = pure
  (>>=) (Left' x) f = f x
  (>>=) (Right' x) f = Right' x

-- 3
newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity $ f x
  
instance Monad Identity where
  (>>=) (Identity x) f = f x

-- 4
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x y) = Cons (f x) (fmap f y)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil x = x
  mappend x Nil = x
  mappend (Cons w x) y = Cons w (mappend x y)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons f x) Nil = Nil
  (<*>) (Cons f x) y = fmap f y `mappend` (x <*> y)

instance Monad List where
  (>>=) Nil _ = Nil
  (>>=) (Cons x y) f = f x `mappend` (y >>= f)

{-Part 2-}

-- 1
j :: Monad m => m (m a) -> m a
j = join

-- 2
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = liftM

-- 3
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

-- 4
a :: Monad m => m a -> m (a -> b) -> m b
a = flip ap

-- 5
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] f = return []
meh (x:xs) f = l2 (:) (f x) (meh xs f) 

-- 6
flipType :: (Monad m) => [m a] -> m [a]
flipType x = meh x (\y -> y >>= return) 
-- flipType x = meh x id
