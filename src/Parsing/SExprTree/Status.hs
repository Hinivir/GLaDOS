{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parsing/SExprTree/Status
-}

module Parsing.SExprTree.Status (
  SExprTree(..),
  SExprTreeOut,
  SExprTreeIn,
  createSExprTreeOutError,
  createSExprTreeOutOK,
  createSExprTreeOutOKForce,
  errorContent,
  parseSExpr
) where

import Parsing.Tokenizer (
  TokenizedAny(..)
  )

import ParserStatus (
  ParserStatus,
  createParserStatusError,
  createParserStatusOk,
  )

import Parsing.SExprTree (
  SExpr(..)
  )

-- TOKENIZER

type SExprTreeIn = [TokenizedAny]
type SExprTreeOut = (SExpr, SExprTreeIn, ParserStatus)

--
data SExprTree =
  SExprTree
    { runSExprTree :: SExprTreeIn -> SExprTreeOut }

--
createSExprTreeOutOKForce :: SExpr -> SExprTreeIn -> SExprTreeOut
createSExprTreeOutOKForce a b = (a, b, createParserStatusOk)

--
createSExprTreeOutOK :: SExpr -> SExprTreeIn -> SExprTreeOut
createSExprTreeOutOK a [] = createSExprTreeOutOKForce a []
createSExprTreeOutOK a (h:t) = createSExprTreeOutOKForce a t

--
createSExprTreeOutError :: (Int, Int) -> String -> String -> SExprTreeOut
createSExprTreeOutError input name info = case input of
  (ln, col) ->
    (SExprUndefined, [], createParserStatusError name info ln col)

--
errorContent :: (SExprTreeOut) -> SExpr -> SExprTreeOut
errorContent (_, input, status) content = (content ,input, status)

-- FUNCTIONS --

--
parseSExpr :: SExprTreeIn -> SExprTree -> SExprTreeOut
parseSExpr a f = runSExprTree f a
