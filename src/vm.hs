{- 
-- EPITECH PROJECT, 2023
-- glados_bootstrap
-- File description:
-- vm.hs
-}

module Value
    ( Value
    ) where

import Data.Char (isDigit)
import Data.Either

data Value = Number Int
            | Boolean Bool
            | Op Operation
            | Func [Instruction]
            deriving (Show, Eq)

data Operation = Add
                | Sub
                | Mul
                | Div
                | Eq
                | Less
                deriving (Show, Eq)

data Instruction = Push Value
                | PushArg Int
                | PushEnv String
                | Call
                | Ret
                | JumpIfFalse Int
                deriving (Show, Eq)

type Args = [Value]
type Stack = [Value]
type Instructions = [Instruction]
type Env = [(String, Instructions)]

resInt :: Either String Value -> Int
resInt (Right (Number x)) = x
resInt _ = 0

resBool :: Either String Value -> Bool
resBool (Right (Boolean x)) = x
resBool _ = False

resOp :: Either String Value -> Operation
resOp (Right (Op x)) = x
resOp _ = Add

callOp :: Operation -> Value -> Value -> Either String Value
callOp Add (Number x) (Number y) = Right (Number(x + y))
callOp Sub (Number x) (Number y) = Right (Number(x - y))
callOp Mul (Number x) (Number y) = Right (Number(x * y))
callOp Div _ (Number 0) = Left "Error: division by zero"
callOp Div (Number x) (Number y) = Right (Number(x `div` y))
callOp Eq (Number x) (Number y) = Right (Boolean(x == y))
callOp Eq (Boolean x) (Boolean y) = Right (Boolean(x == y))
callOp Less (Number x) (Number y) = Right (Boolean(x < y))
callOp _ _ _ = Left "Error: invalid operation"

execFunc :: Args -> Env -> Value -> Stack -> Either String Value
execFunc args env (Func instr) [] = exec args env instr []
execFunc _ _ _ _ = Left "Error: invalid function"

pushFromEnv :: Env -> String -> Either String Instructions
pushFromEnv [] str = Left "Error: function not found"
pushFromEnv ((fc, instr):xs) str
    | fc == str = Right instr
    | otherwise = pushFromEnv xs str

exec :: Args -> Env -> Instructions -> Stack -> Either String Value
exec args env (PushEnv str:xs) stack = do
    z <- pushFromEnv env str
    exec args env xs (Func z:stack)
exec args env (PushArg y:ys) stack = exec args env ys ((args !! y):stack)
exec args env (Push x:xs) stack = exec args env xs (x:stack)
exec args env (Call:xs) (Func f:stack) = do
    z <- execFunc stack env (Func f) []
    exec args env xs (z:stack)
exec args env (Call:xs) (Op op:x:y:stack) = do
    z <- callOp op x y
    exec args env xs (z:stack)
exec args env (Ret:_) (x:_) = Right x
exec args env (JumpIfFalse n:xs) (Boolean False:ys) =
    exec args env (drop n xs) ys
exec args env (JumpIfFalse n:xs) (_:stack) = e xec args env xs stack
exec _ _ _ _ = Left "Invalid instruction"
