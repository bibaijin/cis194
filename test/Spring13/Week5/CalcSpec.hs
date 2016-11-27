module Spring13.Week5.CalcSpec where

import Test.Hspec

import Spring13.Week5.Calc
import Spring13.Week5.ExprT

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  do describe "eval" $
       it "cases" $
       eval (Mul (Add (Lit 2)
                      (Lit 3))
                 (Lit 4)) `shouldBe`
       20
     describe "evalStr" $
       it "cases" $
       do evalStr "(2+3)*4" `shouldBe` Just 20
          evalStr "2+3*4" `shouldBe` Just 14
          evalStr "2+3*" `shouldBe` Nothing
