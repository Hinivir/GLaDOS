{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parsing/Instruct
-}

module Parsing.Instruct (
  LDataType(..),
  Instruct(..)
) where

-- INSTRUCT --

data Instruct = InstructUndefined
  deriving (Eq, Show)

-- | The Instruct data
data LDataType =
  LDataInt Int (Int, Int)
  | LDataFloat Float (Int, Int) 
  deriving (Eq, Show)

