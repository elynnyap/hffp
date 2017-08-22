{- CHAPTER EXERCISES -}

getDigit :: Integral a => a -> a -> a
getDigit x y = d
  where xFirst = fst . divMod x $ y 
        d      = xFirst `mod` 10 

tensDigit :: Integral a => a -> a
tensDigit x = getDigit x 10

hunsDigit :: Integral a => a -> a
hunsDigit x = getDigit x 100

foldBool1 :: a -> a -> Bool -> a
foldBool1 x y b
  | b         = x
  | otherwise = y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b = 
  case b of
    True  -> x
    False -> y

g :: (a -> b) -> (a, c) -> (b, c)
g f (x,y) = (f x, y)
