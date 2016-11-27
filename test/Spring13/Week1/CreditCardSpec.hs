module Spring13.Week1.CreditCardSpec
  (main
  ,spec)
  where

import Test.Hspec

import Spring13.Week1.CreditCard

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "toDigits" $ do
    it "convert digits to single digits" $ do
      toDigits 1234 `shouldBe` [1, 2, 3, 4]
      toDigits 0 `shouldBe` []
      toDigits (-17) `shouldBe` []

  describe "toDigitsRev" $ do
    it "convert digits to single digits and reverse" $ do
      toDigitsRev 1234 `shouldBe` [4, 3, 2, 1]

  describe "doubleEveryOther" $ do
    it "double every other of the int list" $ do
      doubleEveryOther [8, 7, 6, 5] `shouldBe` [16, 7, 12, 5]
      doubleEveryOther [1, 2, 3] `shouldBe` [1, 4, 3]

  describe "sumDigits" $ do
    it "sum digits" $ do
      sumDigits [16, 7, 12, 5] `shouldBe` 22

  describe "validate" $ do
    it "validate credit card" $ do
      validate 4012888888881881 `shouldBe` True
      validate 4012888888881882 `shouldBe` False

