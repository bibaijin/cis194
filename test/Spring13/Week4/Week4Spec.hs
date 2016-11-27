module Spring13.Week4.Week4Spec where

import Test.Hspec
import Test.QuickCheck

import Spring13.Week4.Week4

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  do describe "fun1'" $
       do it "property" $
            do property $ \xs -> fun1' xs == fun1 (xs :: [Integer])
     describe "fun2'" $
       do it "cases" $
            do fun2' 1 `shouldBe` fun2 1
               fun2' 10 `shouldBe` fun2 10
               fun2' 82 `shouldBe` fun2 82
               fun2' 113 `shouldBe` fun2 113
     describe "foldTree" $
       do it "cases" $
            do foldTree [] `shouldBe` (Leaf :: Tree Char)
               foldTree "A" `shouldBe` Node 0 Leaf 'A' Leaf
               foldTree "ABCDEFGHIJ" `shouldBe`
                 Node 3
                      (Node 2
                            (Node 1 (Node 0 Leaf 'D' Leaf) 'G' Leaf)
                            'I'
                            (Node 1 (Node 0 Leaf 'A' Leaf) 'E' Leaf))
                      'J'
                      (Node 2
                            (Node 1 (Node 0 Leaf 'B' Leaf) 'F' Leaf)
                            'H'
                            (Node 0 Leaf 'C' Leaf))
     describe "foldTree'" $
       do it "cases" $
            do foldTree' [] `shouldBe` (Leaf :: Tree Char)
               foldTree' "A" `shouldBe` Node 0 Leaf 'A' Leaf
               foldTree' "ABCDEFGHIJ" `shouldBe`
                 Node 3
                      (Node 2
                            (Node 1 (Node 0 Leaf 'A' Leaf) 'B' Leaf)
                            'C'
                            (Node 1 (Node 0 Leaf 'D' Leaf) 'E' Leaf))
                      'F'
                      (Node 2
                            (Node 1 (Node 0 Leaf 'G' Leaf) 'H' Leaf)
                            'I'
                            (Node 0 Leaf 'J' Leaf))
     describe "xor" $
       do it "cases" $
            do xor [False,True,False] `shouldBe` True
               xor [False,True,False,False,True] `shouldBe` False
          it "property" $
            do property $
                 \xs ->
                   xor xs ==
                   if even (length $
                            filter (\x -> x)
                                   (xs :: [Bool]))
                      then False
                      else True
