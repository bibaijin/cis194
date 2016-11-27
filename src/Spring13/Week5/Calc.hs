{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Spring13.Week5.Calc
  (eval
  ,evalStr
  ,Expr(..)
  ,reify
  ,MinMax(..)
  ,Mod7(..)
  ,compile
  ,compile2)
  where

import           Data.Maybe

import qualified Spring13.Week5.ExprT   as E
import           Spring13.Week5.Parser
import qualified Spring13.Week5.StackVM as S

eval :: E.ExprT -> Integer
eval (E.Lit n)           = n
eval (E.Add expr1 expr2) = eval expr1 + eval expr2
eval (E.Mul expr1 expr2) = eval expr1 * eval expr2

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp E.Lit E.Add E.Mul

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr E.ExprT where
  lit = E.Lit
  add = E.Add
  mul = E.Mul

reify :: E.ExprT -> E.ExprT
reify = id

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit n
    | n <= 0 = False
    | otherwise = True
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax m) (MinMax n) = MinMax (max m n)
  mul (MinMax m) (MinMax n) = MinMax (min m n)

instance Expr Mod7 where
  lit n = Mod7 $ n `mod` 7
  add (Mod7 m) (Mod7 n) = Mod7 $ (m + n) `mod` 7
  mul (Mod7 m) (Mod7 n) = Mod7 $ (m * n) `mod` 7

instance Expr S.Program where
  lit n = [S.PushI n]
  add p1 p2 = p1 ++ p2 ++ [S.Add]
  mul p1 p2 = p1 ++ p2 ++ [S.Mul]

compile :: String -> Maybe S.Program
compile = parseExp lit add mul

compile2 :: String -> Either String S.StackVal
compile2 = S.stackVM . fromMaybe [] . compile
