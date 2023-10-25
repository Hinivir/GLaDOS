{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parsing/LDataTree/TreeToLData
-}

module Parsing.LDataTree.TreeToLData (
    expressTokenizedTree
) where

import ParserStatus (
  ParserStatus,
  createParserStatusError,
  createParserStatusOk,
  isParserStatusError
  )

import Parsing.LDataTree (
  LData(..)
  )

import Parsing.LDataTree.Status (
  LDataTree(LDataTree),
  LDataTreeIn,
  LDataTreeOut,
  parseLData
  )

import Parsing.Tokenizer (
  TokenizedAny(..),
  getTokenizerCoordinates
  )

-- | Parses lists of TokenizedAny, like [1,2,3,4,"Hello!",(+ 4 -2)]
expressList :: LDataTreeIn -> ([LData], ParserStatus)
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
expressGroup :: LDataTreeIn -> ([LData], ParserStatus)
expressGroup list = case expressTokenizedTree list of
  (Nothing, status) -> ([], status)
  (Just x, status)  -> (x, status)

--
expressTokenizedFunc :: LDataTreeIn -> LDataTreeOut
expressTokenizedFunc [] =
  (LDataUndefined, [], createParserStatusOk)
-- TokenizedString
expressTokenizedFunc ((TokenizedString "true" coor):t) =
  ((LDataBool True coor), t, createParserStatusOk)
expressTokenizedFunc ((TokenizedString "false" coor):t) =
  ((LDataBool False coor), t, createParserStatusOk)
expressTokenizedFunc ((TokenizedString x coor):t) =
  ((LDataSymbol x coor), t, createParserStatusOk)
-- TokenizedInt
expressTokenizedFunc ((TokenizedInt x coor):t) =
  ((LDataInt x coor), t, createParserStatusOk)
-- TokenizedFloat
expressTokenizedFunc ((TokenizedFloat x coor):t) =
  ((LDataFloat x coor), t, createParserStatusOk)
-- TokenizedLiteral
expressTokenizedFunc ((TokenizedLiteral x coor):t) =
  ((LDataString x coor), t, createParserStatusOk)
-- TokenizedList
expressTokenizedFunc ((TokenizedList '(' list coor):t) =
  case expressGroup list of
    (output, status)
      | isParserStatusError status  -> (LDataUndefined, [], status)
      | otherwise                   ->
        (LDataGroup output coor, t, createParserStatusOk)
expressTokenizedFunc ((TokenizedList '[' list coor):t) =
  case expressList list of
    (output, status)
      | isParserStatusError status  -> (LDataUndefined, [], status)
      | otherwise                   ->
        (LDataList output coor, t, createParserStatusOk)
expressTokenizedFunc ((TokenizedList x _ coor):t) = case coor of
  (ln, col) -> (LDataUndefined, [], createParserStatusError
    ("Unsupported TokenizedList starting with '" ++ [x] ++ "'")
    "(expressTokenizedFunc)" ln col)
-- TokenizedChar (error)
expressTokenizedFunc ((TokenizedChar x coor):t) = case coor of
  (ln, col) -> (LDataUndefined, [], createParserStatusError
    ("Unsupported TokenizedChar '" ++ [x] ++ "'")
    "(expressTokenizedFunc)" ln col)
--
expressTokenizedFunc (token:t) = case getTokenizerCoordinates token of
  (ln, col) -> (LDataUndefined, [], createParserStatusError
    "Unsupported TokenizedAny" "(expressTokenizedFunc)" ln col)

--
expressTokenized :: LDataTree
expressTokenized = LDataTree $ \input ->
  expressTokenizedFunc input

--
expressTokenizedTree :: [TokenizedAny] -> (Maybe [LData], ParserStatus)
expressTokenizedTree [] = (Just [], createParserStatusOk)
expressTokenizedTree input = case input `parseLData` expressTokenized of
  (output, rest, status)
    | isParserStatusError status  -> (Nothing, status)
    | otherwise                   -> case expressTokenizedTree rest of
      (Just output2, status2)
        | isParserStatusError status2 -> (Nothing, status2)
        | otherwise                   -> (Just (output:output2), status2)
      (_, status2)                    -> (Nothing, status2)
