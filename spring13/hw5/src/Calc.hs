module Calc where
import ExprT
import Parser

import Data.Maybe

eval :: ExprT -> Integer
eval (Lit x)    =  x
eval (Add x y)  =  eval x + eval y
eval (Mul x y)  =  eval x * eval y
            
evalStr :: String -> Maybe Integer
evalStr = handleMaybe . parseExp Lit Add Mul 

handleMaybe :: Maybe a -> Maybe Integer
handleMaybe (Just a) = Just (eval a)
handleMaybe Nothing = Nothing

