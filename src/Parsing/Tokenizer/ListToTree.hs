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
  TokenizedAny(TokenizedChar, TokenizedList)
  )

import Parsing.Tokenizer.Status (
  listUnique,
  listUniquePair,
  listUniquePairEnd,
  listUniquePairStart,
  isUniquePair
  )

import ParserStatus (
  ParserStatus,
  createParserStatusError,
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
isClosingPair :: TokenizedAny -> Char -> Bool
isClosingPair (TokenizedChar c1 _) c2 = isUniquePair c2 c1
isClosingPair _ _ = False

--
tokenizeListToTreeInRec :: TokenListInOutput -> TokenListIn
tokenizeListToTreeInRec (Just output, rest, status) (h:t) sep =
  (Just (h:output), rest, status)
tokenizeListToTreeInRec (_, _, status) _ _ = (Nothing, [], status)

--
tokenizeListToTreeInSegPair :: TokenListIn
tokenizeListToTreeInSegPair ((TokenizedChar c (ln, col)):t) sep =
  case (TokenizedChar c (ln, col)) of
    h -> case tokenizeListToTreeIn t (Just h) of
      (Just output, rest, status) ->
          tokenizeListToTreeIn ((TokenizedList c output (ln, col):rest)) sep
      output                      -> output

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
tokenizeListToTreeIn (h:t) sep = case sep of
  Just (TokenizedChar sepc (ln, col))
    | isClosingPair h sepc  -> (Just [], t, createParserStatusOk)
    | otherwise             -> tokenizeListToTreeInSeg (h:t) sep
  _                         -> tokenizeListToTreeInSeg (h:t) sep

--
tokenizeListToTree :: [TokenizedAny] -> (Maybe [TokenizedAny], ParserStatus)
tokenizeListToTree [] = (Just [], createParserStatusOk)
tokenizeListToTree input = case tokenizeListToTreeIn input Nothing of
  (output, _, status) -> (output, status)
