module HW03 where

--NOTE: You can also assign Integers to variables!
data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

--Function of (String->Int) return type State
type State = String -> Int

-- Exercise 1 -----------------------------------------
-- Store and look up state of a variable
-- Whenever we assign a variable, we want to update the program State
-- Create a new state which maps str with i, otherwise use the old state
extend :: State -> String -> Int -> State
extend state1 str i = state2
    where state2 :: State
          state2 s
            | s==str        = i
            | otherwise     = state1 s 

empty :: State
empty str = 0

-- Exercise 2 -----------------------------------------
-- Results of "True" or "False" will be 1 or 0 respectively

evalE :: State -> Expression -> Int
evalE state (Var str) = 0
evalE state (Val i) = i
evalE state (Op exp1 Plus exp2)       = (evalE state exp1) + (evalE state exp2)
evalE state (Op exp1 Minus exp2)      = (evalE state exp1) - (evalE state exp2)
evalE state (Op exp1 Times exp2)      = (evalE state exp1) * (evalE state exp2)
evalE state (Op exp1 Divide exp2)     = div (evalE state exp1) (evalE state exp2)
evalE state (Op exp1 Gt exp2)         
    | (evalE state exp1) > (evalE state exp2)                   = 1
    | otherwise                                                 = 0
evalE state (Op exp1 Ge exp2)         
    | (evalE state exp1) >= (evalE state exp2)                  = 1
    | otherwise                                                 = 0
evalE state (Op exp1 Lt exp2)         
    | (evalE state exp1) < (evalE state exp2)                   = 1
    | otherwise                                                 = 0
evalE state (Op exp1 Le exp2)         
    | (evalE state exp1) <= (evalE state exp2)                  = 1
    | otherwise                                                 = 0
evalE state (Op exp1 Eql exp2)        
    | (evalE state exp1) == (evalE state exp2)                  = 1
    | otherwise                                                 = 0


-- Exercise 3 -----------------------------------------

-- DietStatement like Statement w/out Incr and For
-- Incr is syntatic sugar for assignment and For is sugar for While loop
data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign a exp)          = DAssign a exp
desugar (Incr a)                = DAssign a (Op (Var a) Plus (Val 1)) 
desugar (If exp st1 st2)        = DIf exp (desugar st1) (desugar st2)
desugar (While exp st)          = DWhile exp (desugar st)
desugar (For st1 exp st2 st3)   = DWhile exp (DSequence (DSequence (desugar st1) (desugar st2)) (desugar st3))
desugar (Sequence st1 st2)      = DSequence (desugar st1) (desugar st2)
desugar (Skip)                  = DSkip

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple state (DAssign str exp) = extend state str (evalE state exp)
evalSimple state (DIf exp diet1 diet2) 
    | (evalE state exp) == 1    = evalSimple state diet1
    | otherwise                 = evalSimple state diet2

run :: State -> Statement -> State
run = undefined

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
