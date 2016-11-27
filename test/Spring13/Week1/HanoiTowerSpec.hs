module Spring13.Week1.HanoiTowerSpec where

import Test.Hspec

import Spring13.Week1.HanoiTower

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "hanoi" $ do
    it "move hanoi tower" $ do
      hanoi 2 "a" "b" "c" `shouldBe` [("a", "c"), ("a", "b"), ("c", "b")]
      (length $ hanoi 15 "a" "b" "c") `shouldBe` (2^15 - 1)
