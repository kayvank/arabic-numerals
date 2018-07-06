import Test.Hspec
import Lib
import Data.Monoid

main :: IO ()
main = hspec $ do
 describe "Arabic Numerals specs" $ do
   it "must correctly represent construct 0" $ 
      value Zero == 0 `shouldBe` True
   it "must correctly construct 3" $ 
      value (Inc(Inc (Inc Zero))) == 3 `shouldBe` True
   it "must correctly add 2 and 3 using monoid mappend" $ 
     value (Inc(Inc(Zero)) `mappend` Inc(Inc(Inc(Zero))))  == 5 `shouldBe` True
   it "must observe addition  commutative property" $ 
     value (Inc(Inc(Zero)) `mappend` Inc(Inc(Inc(Zero))))  ==
        value (Inc(Inc(Zero)) `mappend` Inc(Inc(Inc(Zero))))  `shouldBe` True