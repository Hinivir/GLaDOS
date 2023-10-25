{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parsing/LDataTree/Status
-}

module Parsing.LDataTree.Status (
  LDataTree(..),
  LDataTreeOut,
  LDataTreeIn,
  createLDataTreeOutError,
  createLDataTreeOutOK,
  createLDataTreeOutOKForce,
  errorContent,
  parseLData
) where

import Parsing.Tokenizer (
  TokenizedAny(..)
  )

import ParserStatus (
  ParserStatus,
  createParserStatusError,
  createParserStatusOk,
  )

import Parsing.LDataTree (
  LData(..)
  )

-- LDATATREE

type LDataTreeIn = [TokenizedAny]
type LDataTreeOut = (LData, LDataTreeIn, ParserStatus)

--
data LDataTree =
  LDataTree
    { runLDataTree :: LDataTreeIn -> LDataTreeOut }

--
createLDataTreeOutOKForce :: LData -> LDataTreeIn -> LDataTreeOut
createLDataTreeOutOKForce a b = (a, b, createParserStatusOk)

--
createLDataTreeOutOK :: LData -> LDataTreeIn -> LDataTreeOut
createLDataTreeOutOK a [] = createLDataTreeOutOKForce a []
createLDataTreeOutOK a (h:t) = createLDataTreeOutOKForce a t

--
createLDataTreeOutError :: (Int, Int) -> String -> String -> LDataTreeOut
createLDataTreeOutError input name info = case input of
  (ln, col) ->
    (LDataUndefined, [], createParserStatusError name info ln col)

--
errorContent :: (LDataTreeOut) -> LData -> LDataTreeOut
errorContent (_, input, status) content = (content ,input, status)

-- FUNCTIONS --

--
parseLData :: LDataTreeIn -> LDataTree -> LDataTreeOut
parseLData a f = runLDataTree f a
