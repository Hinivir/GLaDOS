{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parsing/InstructTree/LDataToInstruct
-}

module Parsing.Instruct.LDataToInstruct (
  convertLDataToInstruct
) where

import ParserStatus (
  ParserStatus (ParserStatusError),
  createParserStatusError,
  createParserStatusOk,
  isParserStatusError
  )

import Parsing.Instruct (
  Instruct(..),
  Value(..),
  Operation(..),
  Instruction(..),
  Instructions
  )

import Parsing.Instruct.Status (
  InstructTree(InstructTree),
  InstructTreeIn,
  InstructTreeOut,
  parseInstruct
  )

import Parsing.LDataTree (
  LData(..),
  getLDataCoordinates
  )

convertLDataToInstruct :: [LData] -> (Maybe Instructions, ParserStatus)
convertLDataToInstruct (LDataGroup x (_, _): _) = convertLDataToInstruct x
convertLDataToInstruct (LDataInt x (_, _) : _) =
  (Just [Push (Number x)], createParserStatusOk)
convertLDataToInstruct (LDataFloat x (_, _) : _) =
  (Just [Push (Float x)], createParserStatusOk)
convertLDataToInstruct (LDataBool x (_, _) : _) =
  (Just [Push (Boolean x)], createParserStatusOk)
convertLDataToInstruct (LDataSymbol "add" (_, _) : _) =
  (Just [Push (Op Add)], createParserStatusOk)
convertLDataToInstruct (LDataSymbol "sub" (_, _) : _) =
  (Just [Push (Op Sub)], createParserStatusOk)
convertLDataToInstruct (LDataSymbol "mul" (_, _) : _) =
  (Just [Push (Op Mul)], createParserStatusOk)
convertLDataToInstruct (LDataSymbol "div" (_, _) : _) =
  (Just [Push (Op Div)], createParserStatusOk)
convertLDataToInstruct (LDataSymbol "eq" (_, _) : _) =
  (Just [Push (Op Eq)], createParserStatusOk)
convertLDataToInstruct (LDataSymbol "less" (_, _) : _) =
  (Just[Push (Op Less)], createParserStatusOk)
convertLDataToInstruct (LDataList x (_, _) : _) = convertLDataToInstruct x
convertLDataToInstruct (LDataDict x (_, _) : _) = convertLDataToInstruct x
convertLDataToInstruct (LDataTuple x (_, _) : _) = convertLDataToInstruct x
convertLDataToInstruct (LDataSymbol "Lipdo" (l, c) : _) =
  (Nothing, createParserStatusError "Error" "Lipdo Not Implemented yet" l c)
convertLDataToInstruct (LDataSymbol x (l, c): _) =
  (Nothing, createParserStatusError "Error" ("Symbol Not Know " ++ x) l c)
convertLDataToInstruct (LDataString x (_, _) : _) =
  (Just [Push (String x)], createParserStatusOk)
convertLDataToInstruct [] =
  (Nothing, createParserStatusError "Error" "Can't Convert" 0 0)
convertLDataToInstruct (LDataUndefined : _) =
  (Nothing, createParserStatusError "Error" "Can't Convert" 0 0)

{-- Todo
[LDataGroup [LDataSymbol "Lipdo" (1,1),
LDataSymbol "add" (7,1),
LDataSymbol "a" (11,1),
LDataSymbol "b" (13,1),
LDataSymbol ":" (14,1)] (0,0),
LDataGroup [LDataSymbol "a" (5,2),
LDataSymbol "+" (7,2),
LDataSymbol "b" (9,2)] (0,0),
LDataGroup [LDataSymbol "Lipdo" (1,3),
LDataSymbol "main" (7,3),
LDataSymbol ":" (11,3)] (0,0),
LDataGroup [LDataSymbol "add" (5,4),
LDataInt 3 (9,4),
LDataInt 4 (11,4)] (0,0)]
--}
