module Lecture5 where

main :: IO ()
main = undefined

-- Only 1 kind of implementation allowed?
a1 :: a -> a -> a
a1 x _ = x 
a1 _ y = y 
