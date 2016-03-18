{-# Language FlexibleInstances #-}

module Calc where
import ExprT
import Parser
import qualified StackVM as S

import Data.Maybe

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit x)    =  x
eval (Add x y)  =  eval x + eval y
eval (Mul x y)  =  eval x * eval y

-- Exercise 2
-- Use fmap <$>, to map the eval function to the result of parseExp
evalStr :: String -> Maybe Integer
evalStr x = eval <$> parseExp Lit Add Mul x

-- Exercise 3
-- Create a type class "Expr" that parallels ExprT and create an instance of that class for ExprT
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit 
    add = Add 
    mul = Mul 

-- Exercise 4
-- Create instances of Expr for Integer, Bool, MinMax and Mod7
instance Expr Integer where
    lit x = x
    add = (+)
    mul = (*)

instance Expr Bool where
    lit x = x>0
    add x y = x || y
    mul x y = x && y

newtype MinMax = MinMax Integer deriving (Eq,Show)
instance Expr MinMax where
    lit = MinMax
    add (MinMax x) (MinMax y) = MinMax (max x y)
    mul (MinMax x) (MinMax y) = MinMax (min x y)

newtype Mod7 = Mod7 Integer deriving (Eq,Show)
instance Expr Mod7 where
    lit x = Mod7 (mod x 7)
    add (Mod7 x) (Mod7 y) = Mod7 ( (`mod` 7) $ x+y)
    mul (Mod7 x) (Mod7 y) = Mod7 ( (`mod` 7) $ x*y)

testExpr :: Expr a => Maybe a
testExpr = parseExp lit add mul "(3 * -4) + 5"

-- Exercise 5
-- Build Calculator that emits new assembly language
