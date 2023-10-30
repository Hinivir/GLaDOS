{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parsing/InstructTree/Status
-}

module Parsing.Instruct.Status (
  InstructTree(..),
  InstructTreeOut,
  InstructTreeIn,
  createInstructTreeOutError,
  createInstructTreeOutOK,
  createInstructTreeOutOKForce,
  errorContent,
  parseInstruct
) where

import Parsing.LDataTree (
  LData(..)
  )

import ParserStatus (
  ParserStatus,
  createParserStatusError,
  createParserStatusOk,
  )

import Parsing.Instruct (
  Instruct(..)
  )

-- TOKENIZER

type InstructTreeIn = [LData]
type InstructTreeOut = (Instruct, InstructTreeIn, ParserStatus)

--
data InstructTree =
  InstructTree
    { runInstructTree :: InstructTreeIn -> InstructTreeOut }

--
createInstructTreeOutOKForce :: Instruct -> InstructTreeIn -> InstructTreeOut
createInstructTreeOutOKForce a b = (a, b, createParserStatusOk)

--
createInstructTreeOutOK :: Instruct -> InstructTreeIn -> InstructTreeOut
createInstructTreeOutOK a [] = createInstructTreeOutOKForce a []
createInstructTreeOutOK a (_:t) = createInstructTreeOutOKForce a t

--
createInstructTreeOutError :: (Int, Int) -> String -> String -> InstructTreeOut
createInstructTreeOutError input name info = case input of
  (ln, col) ->
    (InstructUndefined, [], createParserStatusError name info ln col)

--
errorContent :: InstructTreeOut -> Instruct -> InstructTreeOut
errorContent (_, input, status) content = (content ,input, status)

-- FUNCTIONS --

--
parseInstruct :: InstructTreeIn -> InstructTree -> InstructTreeOut
parseInstruct a f = runInstructTree f a
