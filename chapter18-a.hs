import Control.Monad(join)

-- Defining `bind` in terms of `fmap` and `join`
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- join :: Monad m => m (m a) -> m a
bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join $ fmap f x

twiceWhenEven :: [Integer] -> [Integer] 
twiceWhenEven xs = do
  x <- xs 
  if even x
    then [x*x, x*x] 
    else [x*x]

main = print $ bind f x
    where f x = Just $ x ++ "!!!"
          x = Just "hello"
