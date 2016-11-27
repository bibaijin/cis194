module Spring13.Week2.LogAnalysisSpec where

import Test.Hspec

import Spring13.Week2.LogAnalysis
import Spring13.Week2.Log

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseMessage" $ do
    it "parse message" $ do
      parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"
      parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"
      parseMessage "This is not in the right format" `shouldBe` Unknown "This is not in the right format"
