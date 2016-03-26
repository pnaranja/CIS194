module Fibonacci
    where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

fib1 :: [Integer]
fib1 = map fib [0..]
