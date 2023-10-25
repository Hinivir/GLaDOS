{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parsing/Instruct
-}

module Parsing.Instruct (
  Instruct(..)
) where

-- INSTRUCT --

-- | The Instruct data
data Instruct = InstructUndefined
  | InstructLipBe String [Instruct]
  deriving (Eq, Show)
