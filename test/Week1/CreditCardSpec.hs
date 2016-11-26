module Week1.CreditCardSpec
  (main
  ,spec)
  where

import Test.Hspec

import Week1.CreditCard

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "CreditCard.toDigits" $ do
    it "convert digits to single digits" $ do
      toDigits 1234 `shouldBe` [1, 2, 3, 4]
      toDigits 0 `shouldBe` []
      toDigits (-17) `shouldBe` []

  describe "CreditCard.toDigitsRev" $ do
    it "convert digits to single digits and reverse" $ do
      toDigitsRev 1234 `shouldBe` [4, 3, 2, 1]

  describe "CreditCard.doubleEveryOther" $ do
    it "double every other of the int list" $ do
      doubleEveryOther [8, 7, 6, 5] `shouldBe` [16, 7, 12, 5]
      doubleEveryOther [1, 2, 3] `shouldBe` [1, 4, 3]

