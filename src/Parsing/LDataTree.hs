{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parsing/LDataTree
-}

module Parsing.LDataTree (
  LData(..)
) where

-- SEXPR --

-- | The LData data type represents an S-Expression.
data LData = LDataUndefined
  | LDataBool Bool (Int, Int)
  | LDataInt Int (Int, Int)
  | LDataFloat Float (Int, Int)
  | LDataString String (Int, Int)
  | LDataSymbol String (Int, Int)
  | LDataGroup [LData] (Int, Int)
  | LDataList [LData] (Int, Int)
  | LDataDict [LData] (Int, Int)
  | LDataTuple [LData] (Int, Int)
  deriving (Eq, Show)
