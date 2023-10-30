{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parsing/Instruct
-}

module Parsing.Instruct (
  Instruct(..),
  Value(..),
  Operation(..),
  Instruction(..),
  Args,
  Stack,
  Instructions,
  EnvVar(..),
  Env
) where

-- INSTRUCT --

-- | The Instruct data
data Instruct = InstructUndefined
  | InstructLipBe String [Instruct]
  | InstructLipDo String [Args] [Instruct]
  deriving (Eq, Show)

data Value = Number Int
            | Boolean Bool
            | String String
            | Float Float
            | Op Operation
            | Func [Instruction]
            | ValueUndefined
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
                | If Int
                | JumpIfFalse Int
                deriving (Show, Eq)

type Args = [Value]
type Stack = [Value]
type Instructions = [Instruction]
data EnvVar = Var Value
            | Function Instructions
            deriving (Show, Eq)
type Env = [(String, EnvVar)]
