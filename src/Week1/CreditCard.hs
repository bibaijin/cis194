module Week1.CreditCard
  (toDigits
  ,toDigitsRev
  ,doubleEveryOther)
  where

main :: IO ()
main = do
  print $ toDigits 1234
  print $ toDigitsRev 1234
  print $ toDigits 0
  print $ toDigits (-17)

toDigits :: Integer -> [Integer]
toDigits n
  | n > 0 = toDigits (n `div` 10) ++ [n `mod` 10]
  | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n > 0 = (n `mod` 10) : toDigitsRev (n `div` 10)
  | otherwise = []

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith (*) oneTwo . reverse
  where
    oneTwo = 1 : 2 : oneTwo