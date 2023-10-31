{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parsing/Tokenizer/ListToTree
-}

module Parsing.Tokenizer.ListToTree (
  tokenizeListToTree
) where

import Parsing.Tokenizer (
  TokenizedAny(TokenizedChar, TokenizedLine, TokenizedList),
  getTokenizerCoordinates
  )

import Parsing.Tokenizer.Status (
  listUniquePairStart,
  listUniquePairEnd,
  listUniqueEnd,
  isUniquePair
  )

import ParserStatus (
  ParserStatus,
  createParserStatusError,
  createParserStatusErrorSimple,
  createParserStatusOk,
  isParserStatusError
  )

-- DATA

-- Is made of the output, what is left to parse and the status (OK or Error)
type TokenListInOutput = (Maybe [TokenizedAny], [TokenizedAny], ParserStatus)

-- Is made of the input, the current bracket and the output
type TokenListIn = [TokenizedAny] -> Maybe TokenizedAny -> TokenListInOutput

-- TOKENIZE TREE --

--
isClosingPair :: TokenizedAny -> Maybe TokenizedAny -> Bool
isClosingPair (TokenizedChar c1 _) (Just (TokenizedChar c2 _)) =
  isUniquePair c2 c1
isClosingPair (TokenizedChar c1 _) Nothing
  | c1 `elem` listUniqueEnd = True
  | otherwise               = False
isClosingPair _ _ = False

--
tokenizeListToTreeInRec :: TokenListInOutput -> TokenListIn
tokenizeListToTreeInRec (Just output, rest, status) (h:_) _ =
  (Just (h:output), rest, status)
tokenizeListToTreeInRec (_, _, status) _ _ = (Nothing, [], status)

--
tokenizeListToTreeInSegPair :: TokenListIn
tokenizeListToTreeInSegPair ((TokenizedChar c (ln, col)):t) sep =
  case (TokenizedChar c (ln, col)) of
    h -> case tokenizeListToTreeIn t (Just h) of
      (Just output, rest, _) ->
          tokenizeListToTreeIn ((TokenizedList c output (ln, col):rest)) sep
      output                      -> output
tokenizeListToTreeInSegPair (x:_) _ =
  case getTokenizerCoordinates x of
    (ln, col) -> (Nothing, [], createParserStatusError
      "Unhandled case" "(tokenizeListToTreeInSegPair)" ln col)
tokenizeListToTreeInSegPair _ _ =
  (Nothing, [], createParserStatusErrorSimple
    "Unhandled case" "(tokenizeListToTreeInSegPair)")

--
tokenizeListToTreeInSeg :: TokenListIn
tokenizeListToTreeInSeg [] _ =
  (Just [], [], createParserStatusOk)
tokenizeListToTreeInSeg ((TokenizedChar c (ln, col)):t) sep =
  case TokenizedChar c (ln, col) of
    h
      | c `elem` listUniquePairStart  -> tokenizeListToTreeInSegPair (h:t) sep
      | c `elem` listUniquePairEnd    -> (Nothing, [],
        createParserStatusError ("Unopened closing '" ++ [c] ++ "'") "" ln col)
      | otherwise                     ->
        tokenizeListToTreeInRec (tokenizeListToTreeIn t sep) (h:t) sep
tokenizeListToTreeInSeg (h:t) sep =
  tokenizeListToTreeInRec (tokenizeListToTreeIn t sep) (h:t) sep

--
tokenizeListToTreeIn :: TokenListIn
tokenizeListToTreeIn [] (Just (TokenizedChar sep (ln, col))) =
  (Nothing, [],
    createParserStatusError ("Unclosed '" ++ [sep] ++ "'") "" ln col)
tokenizeListToTreeIn [] _ =
  (Just [], [], createParserStatusOk)
tokenizeListToTreeIn (h:t) sep
  | isClosingPair h sep = (Just [], t, createParserStatusOk)
  | otherwise           = tokenizeListToTreeInSeg (h:t) sep

isMaybeTokenizedAnyEmpty :: [TokenizedAny] -> Bool
isMaybeTokenizedAnyEmpty [] = True
isMaybeTokenizedAnyEmpty ((TokenizedLine []):_) = True
isMaybeTokenizedAnyEmpty _ = False

--
tokenizeListToTreeLine :: Maybe [TokenizedAny] -> Maybe [TokenizedAny]
tokenizeListToTreeLine Nothing = Nothing
tokenizeListToTreeLine (Just x)
  | isMaybeTokenizedAnyEmpty x  = Just []
  | otherwise                   = Just [TokenizedLine x]

--
tokenizeListToTreeLineComb :: Maybe [TokenizedAny] -> Maybe [TokenizedAny] ->
  Maybe [TokenizedAny]
tokenizeListToTreeLineComb Nothing _ = Nothing
tokenizeListToTreeLineComb _ Nothing = Nothing
tokenizeListToTreeLineComb (Just x) (Just y)
  | isMaybeTokenizedAnyEmpty x && isMaybeTokenizedAnyEmpty y = Just []
  | isMaybeTokenizedAnyEmpty x  = Just y
  | isMaybeTokenizedAnyEmpty y  = Just [TokenizedLine x]
  | otherwise                   = Just ((TokenizedLine x):y)

--
tokenizeListToTree :: [TokenizedAny] -> (Maybe [TokenizedAny], ParserStatus)
tokenizeListToTree [] = (Just [], createParserStatusOk)
tokenizeListToTree input = case tokenizeListToTreeIn input Nothing of
  (output, [], status)              -> (tokenizeListToTreeLine output, status)
  (output, rest, _)            -> case tokenizeListToTree rest of
    (Nothing, status2)              -> (Nothing, status2)
    (Just output2, status2)
      | isParserStatusError status2 -> (Nothing, status2)
      | otherwise                   ->
        (tokenizeListToTreeLineComb output (Just output2), status2)
