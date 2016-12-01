{-# LANGUAGE FlexibleInstances #-}

module Spring13.Week7.JoinList
  (scoreLine
  ,(+++))
  where

import Data.Monoid

import Spring13.Week7.Sized
import Spring13.Week7.Scrabble
import Spring13.Week7.Buffer
import Spring13.Week7.Editor

main :: IO ()
main = runEditor editor (fromString (unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]) :: (JoinList (Score, Size) String))

data JoinList m a
  = Empty
  | Single m
           a
  | Append m
           (JoinList m a)
           (JoinList m a)
  deriving (Eq,Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
a +++ b = Append (tag a <> tag b) a b

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing
indexJ n (Append b l r)
  | n < 0 || n > getSize (size b) = Nothing
  | n < lSize = indexJ n l
  | otherwise = indexJ (n - lSize) r
  where lSize = getSize $ size $ tag l

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n (Single b a)
  | n <= 0 = Single b a
  | otherwise = Empty
dropJ n (Append b l r)
  | n <= 0 = Append b l r
  | n >= getSize (size b) = Empty
  | n < lSize = dropJ n l +++ r
  | otherwise = dropJ (n - lSize) r
  where lSize = getSize $ size $ tag l

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n l@(Single _ _)
  | n > 0 = l
  | otherwise = Empty
takeJ n l@(Append b l1 l2)
  | n <= 0 = Empty
  | n >= getSize (size b) = l
  | n <= size1 = takeJ n l1
  | otherwise = l1 +++ takeJ (n - size1) l2
  where size1 = getSize $ size $ tag l1

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

jlToList :: JoinList b a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append b l r) = jlToList l ++ jlToList r

instance Buffer (JoinList (Score,Size) String) where
  toString = unlines . jlToList
  fromString = foldr (\s l -> scoreLine' s +++ l) Empty . lines
    where scoreLine' s = Single (scoreString s,Size 1) s
  line = indexJ
  replaceLine n s jl = takeJ n jl +++ fromString s +++ dropJ (n + 1) jl
  numLines = getSize . size . tag
  value = getScore . fst . tag
