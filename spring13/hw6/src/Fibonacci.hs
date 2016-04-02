module Fibonacci
    where

-- Exercise 1
-- Implement Fibonacci numbers
fib :: Integer -> Integer
fib 0  =  0
fib 1  =  1
fib x  =  fib (x-1) + fib (x-2)

fib1 :: [Integer]
fib1  =  map fib [0..]


-- Exercise 2
-- Create a faster way to implement Fibonacci numbers
fib' :: Integer -> [Integer]
fib' 0                  =  [0]
fib' 1                  =  [0,1]
fib' n                  =  xs ++ [xs !! secondprevious + xs !! previous]
    where
        xs              =  fib' (n-1)
        secondprevious  =  fromInteger (n-2)
        previous        =  fromInteger (n-1)

fib2 :: [Integer]
fib2  =  map (last . fib') [0..]

-- Exercise 3
-- Create data type stream and convert stream to List
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a b) = a : streamToList b

instance Show a => Show (Stream a) where
    show (Cons a b) = concatMap (\x-> show x ++ " ") (take 20 (streamToList (Cons a b))) 

-- Exercise 4
-- Write functions streamRepeat, streamMap and streamFromSeed
streamRepeat :: a -> Stream a
streamRepeat a  =  Cons a (streamRepeat a)

streamMap :: (a->b) -> Stream a -> Stream b
streamMap f (Cons a b) = Cons (f a) (streamMap f b)

streamFromSeed :: (a->a) -> a -> Stream a
streamFromSeed f a = Cons (f a) (streamFromSeed f (f a))

-- Exercise 5
-- Define a stream of natural numbers and the "ruler function"
nats :: Stream Integer
nats  =  streamFromSeed (+1) 0

-- Ruler function output: 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, . . .
-- nth element (starting with n=1), is largest power of 2 which evenly divides n 
-- Example: n=1 1/2^0; n=2 2/2^1; n=3 3/2^0; n=4 4/2^2; n=5 5/2^0; n=6 6/2^1; n=7 7/2^0; n=8 8/2^3
-- Patterns: 0, 1, 0, 2, 0, 1, 0 and 3,4,5...
-- Patterns: 0's, 1,2,1,3,1,2,1,4... 
ruler :: Stream Integer
ruler = interLeaveStream interLeave012 interLeave03
    where
        interLeave012 = interLeaveStream (streamRepeat 0) (interLeaveStream (streamRepeat 1) (streamRepeat 2))
        interLeave03 = interLeaveStream (streamRepeat 0) (streamFromSeed (+1) 2)

interLeaveStream :: Stream a -> Stream a -> Stream a
interLeaveStream (Cons a a') (Cons b b') = Cons a (Cons b (interLeaveStream a' b'))

test = Cons 0 (Cons 1 (Cons 0 (Cons 1 (Cons0))))
