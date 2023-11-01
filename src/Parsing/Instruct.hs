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
  Builtin(..),
  Args,
  Stack,
  Instructions,
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
            | Float Float
            | String String
            | Op Operation
            | Builtin Builtin
            | List [Value]
            | Func Instructions
            | ValueUndefined
            deriving (Show, Eq)

data Builtin = Head
            | Tail
            | Len
            deriving (Show, Eq)

data Operation = Add
                | Sub
                | Mul
                | Div
                | Mod
                | Eq
                | Less
                | Greater
                deriving (Show, Eq)

data Instruction = Push Value
                | PushArg Int
                | PushEnv String
                | Call
                | Ret
                | JumpIfFalse Int
                | Jump Int
                deriving (Show, Eq)

type Args = [Value]
type Stack = [Value]
type Instructions = [Instruction]
type Env = [(String, Value)]
