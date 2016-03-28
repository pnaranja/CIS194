module Fibonacci
    where

-- Exercise 1
-- Implement Fibonacci numbers
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

fib1 :: [Integer]
fib1 = map fib [0..]


-- Exercise 2
-- Create a faster way to implement Fibonacci numbers
fib' :: Integer -> [Integer]
fib' 0 = [0]
fib' 1 = [0,1]
fib' n = xs ++ [xs !! fromInteger (n-2) + xs !! fromInteger (n-1)]
    where xs = fib' (n-1)

fib2 :: [Integer]
fib2 = map (last . fib') [0..]

-- Exercise 3
-- Create data type stream and convert stream to List
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList = undefined