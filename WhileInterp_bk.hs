{-
  Name: <Your name here>
  Class: CS 252
  Assigment: HW2
  Date: <Date assignment is due>
  Description: <Describe the program and what it does>
-}


module WhileInterp (
  Expression(..),
  Binop(..),
  Value(..),
  testProgram,
  run
) where

import Data.Map (Map)
import qualified Data.Map as Map

-- We represent variables as strings.
type Variable = String

-- The store is an associative map from variables to values.
-- (The store roughly corresponds with the heap in a language like Java).
type Store = Map Variable Value

data Expression =
    Var Variable                            -- x
  | Val Value                               -- v
  | Assign Variable Expression              -- x := e
  | Sequence Expression Expression          -- e1; e2
  | Op Binop Expression Expression
  | If Expression Expression Expression     -- if e1 then e2 else e3
  | While Expression Expression             -- while (e1) e2
  deriving (Show)

data Binop =
    Plus     -- +  :: Int  -> Int  -> Int
  | Minus    -- -  :: Int  -> Int  -> Int
  | Times    -- *  :: Int  -> Int  -> Int
  | Divide   -- /  :: Int  -> Int  -> Int
  | Gt       -- >  :: Int -> Int -> Bool
  | Ge       -- >= :: Int -> Int -> Bool
  | Lt       -- <  :: Int -> Int -> Bool
  | Le       -- <= :: Int -> Int -> Bool
  deriving (Show)

data Value =
    IntVal Int
  | BoolVal Bool
  deriving (Show)


-- This function will be useful for defining binary operations.
-- The first case is done for you.
-- Be sure to explicitly check for a divide by 0 and throw an error.
applyOp :: Binop -> Value -> Value -> Value
applyOp Plus (IntVal i) (IntVal j) = IntVal $ i + j
applyOp Minus (IntVal i) (IntVal j) = IntVal $ i - j
applyOp Times (IntVal i) (IntVal j) = IntVal $ i * j
applyOp Divide (IntVal i) (IntVal j) = IntVal $ i `div` j
applyOp Gt (IntVal i) (IntVal j) = 
    if i > j then BoolVal True else BoolVal False
applyOp Ge (IntVal i) (IntVal j) = 
    if i >= j then BoolVal True else BoolVal False
applyOp Lt (IntVal i) (IntVal j) = 
    if i < j then BoolVal True else BoolVal False
applyOp Le (IntVal i) (IntVal j) = 
    if i <= j then BoolVal True else BoolVal False        

-- Implement this function according to the specified semantics
evaluate :: Expression -> Store -> (Value, Store)
evaluate (Op o e1 e2) s =
  let (v1,s1) = evaluate e1 s
      (v2,s') = evaluate e2 s1
  in (applyOp o v1 v2, s')
evaluate e s = 
    case e of
    Val v -> (v,s)
    Var x -> case (Map.lookup x s) of
        Just i -> (i,s)
        Nothing -> (BoolVal False,s)
    Sequence x y -> 
        let (v',s') = evaluate x s
            (v2,s2) = evaluate y s'
           in (v2,s2)
--Sequence s1 s2 -> interpret (interpret (env, s1), s2)
 --   Assign var ex -> (BoolVal True, Map' = Map.insert var (evalval ex,s) s)
    If b s1 s2 -> case (evalval b s) of 
        BoolVal True -> evaluate s1 s 
        BoolVal False -> evaluate s2 s
    While b block -> case (evalval b s) of
        BoolVal True -> -- interpet (env, Sequence block s)
              let (block',s') = evaluate block s
              in (evaluate e s')
        BoolVal False -> evaluate b s
    Assign var ex ->
        let (v,s') = evaluate ex s
        in  (v, Map.insert var v s)

evalval :: Expression -> Store -> Value
evalval e s =
    case e of
        Val v -> v

-- Evaluates a program with an initially empty state
run :: Expression -> (Value, Store)
run prog = evaluate prog Map.empty

-- The same as run, but only returns the Store
testProgram :: Expression -> Store
testProgram prog = snd $ run prog

