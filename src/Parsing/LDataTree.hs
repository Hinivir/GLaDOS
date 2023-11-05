{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parsing/LDataTree
-}

module Parsing.LDataTree (
  LData(..),
  getLDataCoordinates
) where

-- LData --

-- | The LData data type
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

--
getLDataCoordinates :: LData -> (Int, Int)
getLDataCoordinates (LDataBool _ coor) = coor
getLDataCoordinates (LDataInt _ coor) = coor
getLDataCoordinates (LDataFloat _ coor) = coor
getLDataCoordinates (LDataString _ coor) = coor
getLDataCoordinates (LDataSymbol _ coor) = coor
getLDataCoordinates (LDataGroup _ coor) = coor
getLDataCoordinates (LDataList _ coor) = coor
getLDataCoordinates (LDataDict _ coor) = coor
getLDataCoordinates (LDataTuple _ coor) = coor
getLDataCoordinates _ = (0, 0)
