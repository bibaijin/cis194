module Week3.GolfSpec where

import Test.Hspec
import Test.QuickCheck

import Week3.Golf

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "skips" $ do
    it "cases" $ do
      skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]
      skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
      skips [1] `shouldBe` [[1]]
      skips [True, False] `shouldBe` [[True, False], [False]]
      skips [] `shouldBe` ([] :: [[Integer]])
    it "property" $ do
      property $ \xs -> length (skips xs) == length (xs :: [Integer])
  describe "localMaxima" $ do
    it "cases" $ do
      localMaxima [2, 9, 5, 6, 1] `shouldBe` [9, 6]
      localMaxima [2, 3, 4, 1, 5] `shouldBe` [4]
      localMaxima [1, 2, 3, 4, 5] `shouldBe` []
