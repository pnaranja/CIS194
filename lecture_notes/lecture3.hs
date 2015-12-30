import Data.Char (toUpper) -- only uses the toUpper function

--Enumeration type
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

getAge :: Person -> Int
getAge (Person _ a _) = a
