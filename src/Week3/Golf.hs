module Week3.Golf
  (skips
  ,localMaxima
  ,histogram)
  where

main :: IO ()
main = do
  putStrLn $ histogram [1, 1, 1, 5]
  putStrLn $ histogram [1,4,5,4,6,6,3,4,2,4,9]

skips :: [a] -> [[a]]
skips list = [each i list | i <- [1..length list]]

each :: Int -> [a] -> [a]
each n list = [list !! i | i <- [n-1, n-1+n..length list - 1]]

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs)
  | x < y && y > z = y : localMaxima (z:zs)
  | otherwise = localMaxima (y:z:zs)
localMaxima _ = []

histogram :: [Integer] -> String
histogram xs = unlines (map (line c) [m+1,m..1]) ++ "==========\n0123456789\n"
  where
    c = count xs
    m = maximum c

line :: [Int] -> Int -> String
line xs n = [if i >= n then '*' else ' ' | i <- xs]

count :: [Integer] -> [Int]
count xs = map (\n -> length $ filter (== n) xs) [0..9]
