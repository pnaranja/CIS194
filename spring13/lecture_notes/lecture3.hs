--Recursion Patterns

data IntList = Empty | Cons Int IntList
    deriving Show

absAll :: IntList -> IntList
absAll Empty = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs)

squareAll :: IntList -> IntList
squareAll Empty = Empty
squareAll (Cons x xs) = Cons (x*x) (squareAll xs)

-- Extract the common things from the above two functions to create mapIntList function
mapIntList :: (Int -> Int) -> IntList -> Int
mapIntList f Empty = Empty
mapIntList f (Cons x xs) = Cons (f x) (mapIntList xs)

-- Let's Test!
exampleList = Cons (-1) (Cons 2 (Cons (-6) Empty))

addOne x = x+1
squareIt x = x*x

mapAddOneList = mapIntList addOne exampleList
mapSquareItList = mapIntList squareIt exampleList

keepOnlyEven :: IntList -> IntList
keepOnlyEven Empty = Empty
keepOnlyEven (Cons x xs)
    | even x            = Cons x (keepOnlyEven xs)
    | otherwise         = keepOnlyEven xs

even :: Int -> Bool
even x
    | x%2 == 0          = True
    | otherwise         = False
