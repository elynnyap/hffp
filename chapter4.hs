{-
 -Type constructor: Mood
 -Data constructors: Blah, Woot
 -}
data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah

{- CHAPTER EXERCISES -}

awesome = ["Papuchon", "curry", ":)"] 
alsoAwesome = ["Quake", "The Simons"] 
allAwesome = [awesome, alsoAwesome]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome str
  | reverse str == str = True
  | otherwise = False

myAbs :: Integer -> Integer
myAbs x = if x >= 0 then x else (-x)
