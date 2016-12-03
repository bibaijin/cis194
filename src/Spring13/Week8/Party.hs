module Spring13.Week8.Party (maxFun) where

import           Data.List
import           Data.Tree
import           System.Environment

import           Spring13.Week8.Employee

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (empFun e + f)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL es1 f1) (GL es2 f2) = GL (es1 ++ es2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun a b = if a >= b then a else b

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x ts) = f x $ map (treeFold f) ts

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss xs = (withBoss,withoutBoss)
  where withBoss = glCons boss $ foldr (mappend . snd) mempty xs
        withoutBoss = mconcat $ map (uncurry moreFun) xs

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

main :: IO ()
main = getArgs
  >>= readFile . head
  >>= putStrLn . parse . maxFun . read

parse :: GuestList -> String
parse (GL es f) =
  "Total fun: " ++
  show f ++ "\n" ++ unlines (sortOn fisrtName $ map empName es)

fisrtName :: String -> String
fisrtName = head . words
