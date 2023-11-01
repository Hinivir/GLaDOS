{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parsing/Tokenizer/ListOfString/Int
-}

module Parsing.Tokenizer.ListOfString.Int (
  tokenizeInt
) where

import ParserStatus (
  isParserStatusError
  )

import Parsing.Tokenizer.Status (
  Tokenizer(..),
  TokenizerIn,
  TokenizerOut,
  errorContent,
  createTokenizerOutError,
  createTokenizerOutOKForce,
  headTokenizerIn,
  headOfShiftedTokenizerIn,
  listSymbols,
  listOperators,
  listNumDecimal,
  listNumDigits,
  shiftedTokenizerIn,
  signTokenized,
  tokenize
  )

import Parsing.Tokenizer (
  TokenizedAny(TokenizedInt, TokenizedLine)
  )

import Parsing.Tokenizer.ListOfString.Dec (
  tokenizeDec
  )

import Parsing.Tokenizer.ListOfString.Operator (
  tokenizeOperator
  )

--
charToInt :: Char -> Int
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9
charToInt _ = 0

--
tokenizeUIntSegChainOnError :: TokenizerIn -> Char -> (TokenizerOut, Int) ->
  (TokenizerOut, Int)
tokenizeUIntSegChainOnError input c (((TokenizedInt _ _), input2, _), _)
  | (headOfShiftedTokenizerIn input) `elem` listSymbols &&
    not ((headOfShiftedTokenizerIn input) `elem` listOperators) =
      (createTokenizerOutError (shiftedTokenizerIn input)
        ("Unreconized Symbol '" ++ [headOfShiftedTokenizerIn input] ++ "'")
        "(tokenizeInt) Is part of listSymbols", 0)
  | otherwise = (createTokenizerOutOKForce
    (TokenizedInt (charToInt c) (signTokenized input)) input2, 10)
tokenizeUIntSegChainOnError input c ((_, input2, _), _) =
    (createTokenizerOutOKForce
      (TokenizedInt (charToInt c) (signTokenized input)) input2, 10)

--
tokenizeUIntSegChain :: TokenizerIn -> Char -> (TokenizerOut, Int) ->
  (TokenizerOut, Int)
tokenizeUIntSegChain input c (((TokenizedInt x coor), input2, status), tilt)
  | isParserStatusError status  =
    tokenizeUIntSegChainOnError input c
      (((TokenizedInt x coor), input2, status), tilt)
  | otherwise                   =
    (createTokenizerOutOKForce
      (TokenizedInt ((charToInt c) * tilt + x) (signTokenized input)) input2,
        tilt * 10)
tokenizeUIntSegChain input _ ((x, input2, status), tilt)
  | isParserStatusError status  = ((x, input2, status), tilt)
  | otherwise                   =
    (createTokenizerOutError input "Invalid output"
      "(tokenizeInt) tokenizeInt didn't return TokenizedInt", 0)

--
tokenizeUIntSeg :: TokenizerIn -> Char -> (TokenizerOut, Int)
tokenizeUIntSeg input c
  | c `elem` listNumDigits  =
    tokenizeUIntSegChain input c (tokenizeUIntSeg
      (shiftedTokenizerIn input) (headOfShiftedTokenizerIn input))
  | otherwise               =
    ((createTokenizerOutError input ("Unreconized Symbol '" ++ [c] ++ "'")
      "(tokenizeInt) Is not part of listNumDigits")
      `errorContent` (TokenizedInt 0 (signTokenized input)), 0)

--
tokenizeIntSeg :: TokenizerIn -> Char -> TokenizerOut
tokenizeIntSeg input '+' = case tokenizeUIntSeg (shiftedTokenizerIn input)
  (headOfShiftedTokenizerIn input) of
  ((TokenizedInt x _, input2, status), _) ->
    (TokenizedInt x (signTokenized input), input2, status)
  (output, _) -> output
tokenizeIntSeg input '-' = case tokenizeUIntSeg (shiftedTokenizerIn input)
  (headOfShiftedTokenizerIn input) of
  ((TokenizedInt x _, input2, status), _) ->
    (TokenizedInt (0 - x) (signTokenized input), input2, status)
  (output, _) -> output
tokenizeIntSeg input c
  | c `elem` listNumDecimal = (shiftedTokenizerIn input) `tokenize` tokenizeDec
  | c `elem` listNumDigits  = case tokenizeUIntSeg input c of
    (output, _) -> output
  | otherwise               =
    createTokenizerOutError input ("Unreconized Symbol '" ++ [c] ++ "'")
      "(tokenizeInt) Is not '+', '-', '.' or part of listNumDigits"

-- | Handles ints and floats, like 7, -42, -1.6180 or .31415
tokenizeInt :: Tokenizer
tokenizeInt = Tokenizer $ \input ->
  case tokenizeIntSeg input (headTokenizerIn input) of
    (x1, input1, status1)
      | (isParserStatusError status1) &&
        (headTokenizerIn input) `elem` listOperators  ->
          input `tokenize` tokenizeOperator
      | (headTokenizerIn input1) `elem` listOperators ->
        case input1 `tokenize` tokenizeOperator of
          (x2, input2, status2) -> (TokenizedLine [x1, x2], input2, status2)
      | otherwise                                     -> (x1, input1, status1)
