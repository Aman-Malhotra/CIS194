module HW03 where

import Data.Bool

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

type State = String -> Int 

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend initState name val a             = bool (initState a) val (a == name)

empty :: State
empty _                                 = 0

-- Exercise 2 -----------------------------------------

toInt :: Bool -> Int
toInt b = if b then 1 else 0 

evalE :: State -> Expression -> Int
evalE _ (Val a) = a 
evalE initState (Op exp1 bop exp2) = 
  let a = evalE initState exp1 
      b = evalE initState exp2
  in case bop 
    of  Plus            -> a + b
        Minus           -> a - b
        Times           -> a * b
        Divide          -> a `div` b
        Gt              -> toInt $ a > b
        Ge              -> toInt $ a >= b
        Lt              -> toInt $ a < b
        Le              -> toInt $ a <= b 
        Eql             -> toInt $ a == b
evalE _ _ = 0

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign s e)                    = DAssign s e 
desugar (Incr s)                        = DAssign s (Op (Var s) Plus (Val 1))
desugar (If exp1 st1 st2)               = DIf exp1 (desugar st1) (desugar st2)
desugar (Sequence st1 st2)              = DSequence (desugar st1) (desugar st2)
desugar (While exp1 st1)                = DWhile exp1 (desugar st1)
desugar (For ini cond incr body)        = DSequence (desugar ini) (DWhile cond (DSequence (desugar body) (desugar incr)))
desugar _                               = undefined

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple initState (DAssign name exp1)       = extend initState name (evalE initState exp1)
evalSimple initState (DIf check st1 st2)       = evalSimple initState (if evalE initState check == 1 then st1 else st2)
evalSimple initState (DWhile exp1 st1)         = if evalE initState exp1 == 1 then evalSimple initState st1 else initState
evalSimple initState (DSequence st1 st2)       = evalSimple (evalSimple initState st1) st2
evalSimple _ _                                 = empty

run :: State -> Statement -> State
run initState st1 = evalSimple initState (desugar st1)

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