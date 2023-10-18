{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Ast
-}

module Vm () where

import Data.List ()

data Value
    = Number Int
    | Boolean Bool
    | Ope Operation
    deriving (Show, Eq)

data Operation
    = Add
    | Sub
    | Mul
    | Div
    | Less
    | Eq
    deriving (Show, Eq)

data Instruction
    = Ret
    | Push Value
    | Call
    | JumpIfFalse Int
    | PushArgs Int
    deriving (Show, Eq)

type Args = [Value]
type Stack = [Value]
type Insts = [Instruction]

exec :: Args -> Insts -> Stack -> Either String Value
exec _ (Ret:_) [x] = Right x
exec args (Push x:xs) stack = exec args xs (x : stack)
exec args (Call:xs) (Ope op:x:y:stack) = do
    v <- callOp op x y
    exec args xs (v:stack)
exec args (JumpIfFalse n:xs) (Boolean False : stack) = exec args (drop n xs) stack
exec args (JumpIfFalse _:xs) (_ : stack) = exec args xs stack
exec args (PushArgs y:ys) stack = exec args ys (args !! y : stack)
exec _ [] [] = Left "Error: No instructions left to execute"
exec _ _ _ = Left "Error: Invalid instruction"

callOp :: Operation -> Value -> Value -> Either String Value
callOp Add (Number x) (Number y) = Right (Number (x + y))
callOp Sub (Number x) (Number y) = Right (Number (x - y))
callOp Mul (Number x) (Number y) = Right (Number (x * y))
callOp Div _ (Number 0) = Left "Error: Division by zero"
callOp Div (Number x) (Number y) = Right (Number (x `div` y))
callOp Less (Number x) (Number y) = Right (Boolean (x < y))
callOp Eq (Number x) (Number y) = Right (Boolean (x == y))
callOp Eq (Boolean x) (Boolean y) = Right (Boolean (x == y))
callOp _ _ _ = Left "Error: Invalid operation"
 