module Lib  where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data ANumeral  = Zero | Inc ANumeral 

value :: ANumeral -> Int
value Zero = 0
value  (Inc x) = 1 + value x 

value' :: Int -> ANumeral
value' 0 = Zero
value' x = Inc(value' (x - 1) )

add' :: ANumeral -> ANumeral -> ANumeral
add' m1 m2 = m1 `mappend` m2

instance Eq ANumeral where
   a1 == a2 = value(a1) == value(a2)

instance Ord ANumeral where
  compare a1 a2 = compare (value a1) (value a2)
 
instance Monoid (ANumeral ) where
  mempty = Zero 
  mappend Zero x =  x
  mappend (Inc y) x =  mappend y (Inc x)
  
instance Show ANumeral where
  show x = show (value x)
