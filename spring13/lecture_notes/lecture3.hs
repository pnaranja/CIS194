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

-- Polymorphic Data Types
-- Empty -> E and Cons -> C
-- Given a type t, a (List t) is either constructor E or constructor C of type t and another List t
data List t = E | C t (List t)

lst1 :: List Int
lst1 = C 3 (C 5 (C 2 E))

lst2 :: List Char
lst2 = C 'x' (C 'y' (C 'z' E))

lst3 :: List Bool
lst3 = C True (C False E)

-- Make filterIntList
-- Use p instead of f, since f is already used
filterList _ E = E
filterList p (C x xs)
    | p x           = C (p x) (filterList xs)   
    | otherwise     = filterList p xs

