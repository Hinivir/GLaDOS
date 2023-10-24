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

-- | Parses lists of TokenizedAny, like [1,2,3,4,"Hello!",(+ 4 -2)]
expressList :: SExprTreeIn -> ([SExpr], ParserStatus)
expressList [] = ([], createParserStatusOk)
expressList (token:[]) = case expressTokenizedFunc [token] of
  (x, rest, status)
    | isParserStatusError status  -> ([], status)
    | otherwise                   -> ([x], status)
expressList (_:((TokenizedChar ',' coor):[])) = case coor of
  (ln, col) -> ([], createParserStatusError
    "Trailing separator ','" "(expressList)" ln col)
expressList (token:((TokenizedChar ',' _):t)) =
  case expressTokenizedFunc [token] of
    (x, rest, status)
      | isParserStatusError status      -> ([], status)
      | otherwise                       -> case expressList t of
        (list, status2)
          | isParserStatusError status2 -> ([], status2)
          | otherwise                   -> ((x:list), status2)
expressList (_:(token:_)) = case getTokenizerCoordinates token of
  (ln, col) -> ([], createParserStatusError
    "Unsupported group separator" "(expressList)" ln col)

-- | Parses simple groups of TokenizedAny, like for functions
expressGroup :: SExprTreeIn -> ([SExpr], ParserStatus)
expressGroup list = case expressTokenizedTree list of
  (Nothing, status) -> ([], status)
  (Just x, status)  -> (x, status)

--
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
-- TokenizedLiteral
expressTokenizedFunc ((TokenizedLiteral x coor):t) =
  ((SExprString x coor), t, createParserStatusOk)
-- TokenizedList
expressTokenizedFunc ((TokenizedList '(' list coor):t) =
  case expressGroup list of
    (output, status)
      | isParserStatusError status  -> (SExprUndefined, [], status)
      | otherwise                   ->
        (SExprGroup output coor, t, createParserStatusOk)
expressTokenizedFunc ((TokenizedList '[' list coor):t) =
  case expressList list of
    (output, status)
      | isParserStatusError status  -> (SExprUndefined, [], status)
      | otherwise                   ->
        (SExprList output coor, t, createParserStatusOk)
expressTokenizedFunc ((TokenizedList x _ coor):t) = case coor of
  (ln, col) -> (SExprUndefined, [], createParserStatusError
    ("Unsupported TokenizedList starting with '" ++ [x] ++ "'")
    "(expressTokenizedFunc)" ln col)
-- TokenizedChar (error)
expressTokenizedFunc ((TokenizedChar x coor):t) = case coor of
  (ln, col) -> (SExprUndefined, [], createParserStatusError
    ("Unsupported TokenizedChar '" ++ [x] ++ "'")
    "(expressTokenizedFunc)" ln col)
--
expressTokenizedFunc (token:t) = case getTokenizerCoordinates token of
  (ln, col) -> (SExprUndefined, [], createParserStatusError
    "Unsupported TokenizedAny" "(expressTokenizedFunc)" ln col)

--
expressTokenized :: SExprTree
expressTokenized = SExprTree $ \input ->
  expressTokenizedFunc input

--
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
