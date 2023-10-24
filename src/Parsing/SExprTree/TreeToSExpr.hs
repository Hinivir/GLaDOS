{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parsing/SExprTree/TreeToSExpr
-}

module Parsing.SExprTree.TreeToSExpr (
    expressTokenizedTree
) where

import ParserStatus (
  ParserStatus,
  createParserStatusError,
  createParserStatusOk,
  isParserStatusError
  )

import Parsing.SExprTree (
  SExpr(..)
  )

import Parsing.SExprTree.Status (
  SExprTree(SExprTree),
  SExprTreeIn,
  SExprTreeOut,
  parseSExpr
  )

import Parsing.Tokenizer (
  TokenizedAny(..),
  getTokenizerCoordinates
  )

expressTokenizedFunc :: SExprTreeIn -> SExprTreeOut
expressTokenizedFunc [] =
  (SExprUndefined, [], createParserStatusOk)
-- TokenizedString
expressTokenizedFunc ((TokenizedString "true" coor):t) =
  ((SExprBool True coor), t, createParserStatusOk)
expressTokenizedFunc ((TokenizedString "false" coor):t) =
  ((SExprBool False coor), t, createParserStatusOk)
expressTokenizedFunc ((TokenizedString x coor):t) =
  ((SExprSymbol x coor), t, createParserStatusOk)
-- TokenizedInt
expressTokenizedFunc ((TokenizedInt x coor):t) =
  ((SExprInt x coor), t, createParserStatusOk)
-- TokenizedFloat
expressTokenizedFunc ((TokenizedFloat x coor):t) =
  ((SExprFloat x coor), t, createParserStatusOk)
--
expressTokenizedFunc (token:t) = case getTokenizerCoordinates token of
  (ln, col) -> (SExprUndefined, [], createParserStatusError
    "Unreconized TokenizedAny" "" ln col)

expressTokenized :: SExprTree
expressTokenized = SExprTree $ \input ->
  expressTokenizedFunc input

expressTokenizedTree :: [TokenizedAny] -> (Maybe [SExpr], ParserStatus)
expressTokenizedTree [] = (Just [], createParserStatusOk)
expressTokenizedTree input = case input `parseSExpr` expressTokenized of
  (output, rest, status)
    | isParserStatusError status  -> (Nothing, status)
    | otherwise                   -> case expressTokenizedTree rest of
      (Just output2, status2)
        | isParserStatusError status2 -> (Nothing, status2)
        | otherwise                   -> (Just (output:output2), status2)
      (_, status2)                    -> (Nothing, status2)
