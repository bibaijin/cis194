module Spring13.Week4.Week4
  (fun1
  ,fun1'
  ,fun2
  ,fun2'
  ,Tree(Leaf, Node)
  ,foldTree
  ,foldTree'
  ,xor
  ,map'
  ,sieveSundaram)
  where

import Data.List

main :: IO ()
main = do
  putStrLn $ show $ foldTree "ABCDEFGHIJ"
  putStrLn $ show $ foldTree' "ABCDEFGHIJ"

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (\x -> ((x-2) *)) 1 . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' =
  sum .
  filter even .
  takeWhile (/= 1) .
  iterate (\n ->
             if even n
                then (n `div` 2)
                else (3 * n + 1))

data Tree a
  = Leaf
  | Node Integer
         (Tree a)
         a
         (Tree a)
  deriving (Show,Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf

insertTree :: a -> Tree a -> Tree a
insertTree v Leaf = Node 0 Leaf v Leaf
insertTree v (Node h l x r)
  | getHeight l < getHeight r = Node h (insertTree v l) x r
  | getHeight l == getHeight r = Node h' l' x r
  | otherwise = Node h l x $ insertTree v r
  where getHeight Leaf = -1
        getHeight (Node h _ _ _) = h
        l' = insertTree v l
        h' = 1 + getHeight l'

foldTree' :: [a] -> Tree a
foldTree' [] = Leaf
foldTree' xs =
  Node height
       (foldTree' $ take half xs)
       (xs !! half)
       (foldTree' $ drop (half + 1) xs)
  where len = length xs
        half = len `div` 2
        height = floor (logBase 2 (fromIntegral len) :: Double)

xor :: [Bool] -> Bool
xor = foldr xor' False
  where xor' False False = False
        xor' False True = True
        xor' True False = True
        xor' True True = False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+ 1) . (* 2)) $ [1 .. n] \\ sieve
  where sieve =
          map (\(i,j) -> i + j + 2 * i * j) $
          filter (\(i,j) -> i <= j && (i + j + 2 * i * j <= n)) $
          cartProd [1 .. n]
                   [1 .. n]
