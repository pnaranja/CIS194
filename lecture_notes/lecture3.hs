import Data.Char (toUpper) -- only uses the toUpper function

--Enumeration type
-- data <Type> = <Value1> | <Value2> ...
data Thing = Shoe | Ship | SealingWax | Cabbage | King
    deriving Show -- deriving Show tells GHC to generate code to convert "Things" to String

shoe :: Thing
shoe = Shoe

listO'Things :: [Thing]
listO'Things = [Shoe, Ship, SealingWax, Cabbage, King]

isSmall :: Thing -> Bool
isSmall Ship            = False
isSmall King            = False
isSmall _               = True


data FailableDouble = Failure | OK Double -- OK needs a Double parameter to make it type FailableDouble (Data Constructor)
    deriving Show
    
example1 = Failure
example2 = OK 3.4

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x/y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d) = d

--Data Construnctor
data Person = Person String Int Thing
    deriving Show

richard :: Person
richard = Person "Richard" 32 Ship

stan :: Person
stan = Person "Stan" 94 Cabbage

unknown :: Person
unknown = Person "Unknown" 10 SealingWax

getAge :: Person -> Int
getAge (Person _ a _) = a

--General Algebriac Data Types
-- data AlgDataType = Constr1 Type11 Type12
--                 |   Constr2 Type21
--                 |   Constr3

-- Using the '@'.  Here 'p' is binded to the value of Person
baz :: Person -> String
baz p@(Person n _ _) = "The name of field of (" ++ show p ++ ") is " ++ n

favPerson :: Person -> String
favPerson (Person n 32 _) = n ++" is my favorite person!"
favPerson (Person n _ Cabbage) = n ++" is an ok person"
favPerson (Person n 10 _) = n ++" is just too young"

-- Case expressions -> Matches VALUES!  Use the '|' to match types
failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
                    Failure -> 0
                    OK d    -> d

data LogMessage = LogMessage Int String
data MaybeLogMessage = ValidLM LogMessage | InvalidLM
data MaybeInt = ValidInt Int | InvalidInt

--Type Constructor
-- data Maybe a = Just a | Nothing
-- 'a' is a type variable.

example_a :: Maybe Int -> Int
example_a (Just a) = a
example_a Nothing = -1

example_b :: LogMessage -> Maybe String
example_b (LogMessage severity s) | severity >= 50  = Just s
example_b _                                         = Nothing

-- Recursive Data Types
data List' t = Empty | Cons t (List' t)
    deriving Show

lst1 :: List' Int
lst1 = Cons 3 (Cons 4 (Cons 5 Empty))

lst2 :: List' Char
lst2 = Cons 'a' (Cons 'b' (Cons 'c' Empty))

lst3 :: List' Bool
lst3 = Cons True (Cons False (Cons True Empty))

--Use recursive functions to process recursive data types
intListSum :: List' Int -> Int
intListSum Empty           = 0
intListSum (Cons x y)      = x + (intListSum y)

-- BTree example
data Btree a = Leaf a
                | Branch a (Btree a) (Btree a)
                deriving Show

btree1 :: Btree Int
btree1 = Branch 1 (Branch 2 (Leaf 3) (Leaf 4)) (Branch 5 (Leaf 6) (Leaf 7))
--                  1
--          2               5
--     3        4       6       7 
