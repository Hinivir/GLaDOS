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
  ParserStatus,
  createParserStatusError,
  createParserStatusOk,
  isParserStatusError
  )

import Parsing.Instruct (
  Instruct(..),
  Value(..),
  Operation(..),
  Instruction(..),
  Args,
  Stack,
  Instructions,
  EnvVar(..),
  Env
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

--
convertLDataToInstruct :: [LData] -> (Maybe Instructions, ParserStatus)
convertLDataToInstruct  = (Just [], createParserStatusOk)
convertLDataToInstruct _ = (Just [], createParserStatusOk)
