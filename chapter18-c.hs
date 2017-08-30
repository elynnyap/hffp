f :: Maybe Integer
f = Just 1

g :: Maybe String
g = Just "1"

h :: Maybe Integer
h = Just 10191

zed :: a -> b -> c -> (a,b,c)
zed = (,,)

doSomething = do
  a <- f
  b <- g
  c <- h
  return (zed a b c)

zed' :: Monad m => a -> b -> c -> m (a,b,c)
zed' a b c = return (a,b,c)

doSomething' = do
  a <- f
  b <- g
  c <- h
  zed' a b c
