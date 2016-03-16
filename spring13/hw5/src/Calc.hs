module Calc where
import ExprT
import Parser

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
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit 
    add = Add 
    mul = Mul 
