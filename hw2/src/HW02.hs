{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches [] []              = 0
exactMatches _ []              = 0
exactMatches [] _              = 0
exactMatches (x1:xs1) (x2:xs2)
    |   x1 == x2                = 1 + exactMatches xs1 xs2
    |   otherwise               = exactMatches xs1 xs2

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times it occurs in ys (colors)
-- [Int] shall always be length of 6
countColors :: Code -> [Int]
countColors [] = [0, 0, 0, 0, 0, 0]
countColors a = map (\peg->countColor peg a) colors 

-- Helper function: counts a peg color in the code
countColor :: Peg -> Code -> Int
countColor col cod = length $ filter (\x->x==col) cod

-- Main Function: Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches c1 c2 = foldl1 (+) (map getMatch (activeTuples $ getTuples c1 c2))

-- Helper function: creates peg color tuples from the two Codes
getTuples :: Code -> Code -> [(Int,Int)]
getTuples c1 c2 = zip (countColors c1) (countColors c2)

-- Helper function: Filters out (0,0) tuples
activeTuples :: [(Int,Int)] -> [(Int,Int)]
activeTuples = filter ( \ (x,y) -> x>0 && y>0) 

-- Helper function: Determines the number of matches given the tuple
getMatch :: (Int,Int) -> Int
getMatch (x,y)
    | x==y              = x
    | x<y               = x
    | x>y               = y
    | otherwise         = 0

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove c1 c2 = Move c2 m1 m2
    where m1 = exactMatches c1 c2
          m2 = (matches c1 c2) - m1

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent m c = (extractMatchesFromMove m) ==
                    (extractMatchesFromMove (getMove (extractCodeFromMove m) c))

extractMatchesFromMove :: Move -> (Int,Int)
extractMatchesFromMove (Move _ i1 i2) = (i1,i2)

extractCodeFromMove :: Move -> Code
extractCodeFromMove (Move x _ _) = x
extractCodeFromMove (Move _ _ _) = []

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes = undefined

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes = undefined

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
