{-
-- EPITECH PROJECT, 2023
-- glados_bootstrap
-- File description:
-- vm.hs
-}

module Vm
    ( Value(..),
        Operation(..),
        Instruction(..),
        Builtin(..),
        Args,
        Stack,
        Instructions,
        Env,
        resInt,
        resBool,
        resOp,
        callOp,
        execFunc,
        exec
    ) where

import Data.Either()

data Value = Number Int
            | Float Float
            | Boolean Bool
            | Char Char
            | Op Operation
            | Builtin Builtin
            | List [Value]
            | String String
            | Func Instructions Int
            deriving (Read, Show, Eq)

data Builtin = Head
            | Tail
            | Len
            deriving (Read, Show, Eq)

data Operation = Add
                | Sub
                | Mul
                | Div
                | Mod
                | Eq
                | Less
                | LessEq
                | Greater
                | GreaterEq
                | Diff
                deriving (Read, Show, Eq)

data Instruction = Push Value
                | PushArg Int
                | PushEnv String
                | Call
                | Ret
                | JumpIfFalse Int
                | Jump Int
                deriving (Read, Show, Eq)

type Args = [Value]
type Stack = [Value]
type Instructions = [Instruction]
type Env = [(String, Value)]

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
callOp Add (Number x) (Number y) = Right (Number(y + x))
callOp Sub (Number x) (Number y) = Right (Number(y - x))
callOp Mul (Number x) (Number y) = Right (Number(y * x))
callOp Div _ (Number 0) = Left "Error: division by zero"
callOp Div (Number x) (Number y) = Right (Number(y `div` x))
callOp Mod _ (Number 0) = Left "Error: modulo by zero"
callOp Mod (Number x) (Number y) = Right (Number(y `mod` x))
callOp Eq (Number x) (Number y) = Right (Boolean(x == y))
callOp Eq (Boolean x) (Boolean y) = Right (Boolean(x == y))
callOp Less (Number x) (Number y) = Right (Boolean(y < x))
callOp LessEq (Number x) (Number y) = Right (Boolean(y <= x))
callOp Greater (Number x) (Number y) = Right (Boolean(x > y))
callOp GreaterEq (Number x) (Number y) = Right (Boolean(y >= x))
callOp Diff (Number x) (Number y) = Right (Boolean(x /= y))
callOp Diff (Boolean x) (Boolean y) = Right (Boolean(x /= y))
callOp _ _ _ = Left "Error: invalid operation"

callBuiltin :: Builtin -> Value -> Either String Value
callBuiltin Head (List (x:_)) = Right x
callBuiltin Head (String (x:_)) = Right (Char x)
callBuiltin Head _ = Left "Error: head of empty list"
callBuiltin Tail (List (_:xs)) = Right (List xs)
callBuiltin Tail (String (_:xs)) = Right (String xs)
callBuiltin Tail _ = Left "Error: tail of empty list"
callBuiltin Len (List xs) = Right (Number (length xs))
callBuiltin Len (String xs) = Right (Number (length xs))
callBuiltin _ _ = Left "Error: invalid builtin"

execFunc :: Args -> Env -> Value -> Stack -> Either String Value
execFunc args env (Func instr _) [] = exec args env instr []
execFunc _ _ _ _ = Left "Error: invalid function"

pushFromEnv :: Env -> String -> Either String Value
pushFromEnv [] v = Left ("Error: unknown variable or function in env: " ++ v)
pushFromEnv ((fc, instr):xs) str
    | fc == str = Right (instr)
    | otherwise = pushFromEnv xs str
--pushFromEnv ((_, Var _):xs) str = pushFromEnv xs str

exec :: Args -> Env -> Instructions -> Stack -> Either String Value
exec args env (PushEnv str:xs) stack =
    case pushFromEnv env str of
        Right instr -> exec args env xs (instr:stack)
        Left err -> Left err
--    exec args env xs (z:stack)
exec args env (PushArg y:ys) stack
    | null args = Left "Error: cannot push argument (empty)"
    | otherwise = exec args env ys ((args !! y):stack)
exec args env (Push x:xs) stack = exec args env xs (x:stack)
-------------------------------------------
exec args env (Call:xs) (Func f nb:stack) = do
    z <- execFunc stack env (Func f nb) []
    exec args env xs (z:stack)
exec args env (Call:xs) (Op op:x:y:stack) = do
    z <- callOp op x y
    exec args env xs (z:stack)
exec args env (Call:xs) (Builtin bi:x:stack) = do
    z <- callBuiltin bi x
    exec args env xs (z:stack)
-------------------------------------------
exec _ _ (Ret:_) (x:_) = Right x
exec args env (JumpIfFalse n:xs) (Boolean False:ys) =
    exec args env (drop n xs) ys
exec args env (JumpIfFalse _:xs) (_:stack) = exec args env xs stack
exec args env (Jump n:xs) stack = exec args env (drop n xs) stack
exec _ _ _ _ = Left ("Invalid instruction")
