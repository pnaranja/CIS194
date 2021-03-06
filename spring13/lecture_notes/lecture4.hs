module Lecture4 where

main :: IO ()
main = undefined

--Lambdas (Anonymous Functions) --
greaterThan100 :: [Integer] -> [Integer]
greaterThan100 []     = []
greaterThan100 (x:xs)
 | x>100              = x : greaterThan100 xs
 | otherwise          = greaterThan100 xs

greaterThan100' :: [Integer] -> [Integer]
greaterThan100' = filter (>100) -- Parial apply an "operator section"


--Functional Composition --
myfoo :: (b->c) -> (a->b) -> (a->c)
myfoo f g = \x -> f (g x)

--myfoo actually represents a function composition!
myfoo' :: (b->c) -> (a->b) -> a -> c
myfoo' f g = f . g


intLengthEvenGreaterThan100 :: [Integer] -> Bool
intLengthEvenGreaterThan100 = even . length . greaterThan100'


-- Currying and partial application --

-- Use a tuple for "2 argument" functions
tupleFunction :: (Int,Int) -> Int
tupleFunction (x,y) = 2*x + y

myCurry :: ((a,b) -> c) -> a -> b -> c
myCurry f x y = f (x,y)
myCurryEx = myCurry (\(x,y) -> 2*x+y) 3 4 -- == 10

-- Take a tuple pair and apply a function to it
myUnCurry :: (a -> b -> c) -> (a,b) -> c
myUnCurry f (x,y) = f x y
myUnCurryEx = myUnCurry (\x y -> 2*x+y) (3,4) -- == 10


-- Partial Apply
myPartial :: Int -> (Int -> Int)
myPartial x = \y -> 2*x+y 
myFunc = myPartial 3
myWhole = myFunc 4 -- == 10


-- Wholemeal programming
foobar :: [Integer] -> Integer
foobar []     = 0
foobar (x:xs)
 | x>3        = (7*x+2) + foobar xs
 | otherwise  = foobar xs

-- We're just filtering elems >3, applying a function to the elems and summing them
foobar' :: [Integer] -> Integer
foobar' = sum . map (\x->7*x+2) . filter (>3)


-- Folds --
sum' :: [Integer] -> Integer
sum' []     = 0
sum' (x:xs) = x + sum' xs

product' :: [Integer] -> Integer
product' []     = 0
product' (x:xs) = x + product' xs

length' :: [Integer] -> Integer
length' []     = 0
length' (_:xs) = 1 + length' xs

myFoldr a f [x,y,z] = f x (f y (f z a))

-- Works only with List length of 3!
sum''      = myFoldr 0 (+)
product''  = myFoldr 1 (*)
length''   = myFoldr 0 (\_ x -> 1+x) -- (\_ x -> 1+x)  :: Num a => r -> a -> a
length'''  = myFoldr 0 (\_-> (1+))   -- (\_ -> (1+))   :: Num a => r -> a -> a
length'''' = myFoldr 0 (const (1+))  -- (const (1+))   :: Num a => b -> a -> a
