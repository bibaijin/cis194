module Week4.Week4Spec where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "fun1'" $ do
    it "property" $ do
      property $ \xs -> fun1' xs == fun1 (xs :: [Integer])
