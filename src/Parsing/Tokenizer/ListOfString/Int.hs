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
  TokenizedAny(TokenizedInt, TokenizedFloat, TokenizedLine)
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

-- | Used by tokenizeUIntSegChainOnError
listOpeartorsDecimal :: [Char]
listOpeartorsDecimal = listOperators ++ listNumDecimal

--
tokenizeUIntSegChainOnError :: TokenizerIn -> Char -> (TokenizerOut, Int) ->
  (TokenizerOut, Int)
tokenizeUIntSegChainOnError input c (((TokenizedInt _ _), input2, _), _)
  | (headOfShiftedTokenizerIn input) `elem` listSymbols &&
    not ((headOfShiftedTokenizerIn input) `elem` listOpeartorsDecimal) =
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
tokenizeUIntSegStart :: TokenizerIn -> Char -> (TokenizerOut, Int)
tokenizeUIntSegStart input c
  | c `elem` listNumDecimal = (createTokenizerOutOKForce
    (TokenizedInt 0 (signTokenized input)) input, 0)
  | otherwise               =
    tokenizeUIntSeg input c

--
tokenizeIntSeg :: TokenizerIn -> Char -> TokenizerOut
tokenizeIntSeg input '+' = case tokenizeUIntSegStart (shiftedTokenizerIn input)
  (headOfShiftedTokenizerIn input) of
  ((TokenizedInt x _, input2, status), _) ->
    (TokenizedInt x (signTokenized input), input2, status)
  (output, _) -> output
tokenizeIntSeg input '-' = case tokenizeUIntSegStart (shiftedTokenizerIn input)
  (headOfShiftedTokenizerIn input) of
  ((TokenizedInt x _, input2, status), _) ->
    (TokenizedInt (0 - x) (signTokenized input), input2, status)
  (output, _) -> output
tokenizeIntSeg input c
  | c `elem` listNumDecimal = createTokenizerOutOKForce
    (TokenizedInt 0 (signTokenized input)) input
  | c `elem` listNumDigits  = case tokenizeUIntSegStart input c of
    (output, _) -> output
  | otherwise               =
    createTokenizerOutError input ("Unreconized Symbol '" ++ [c] ++ "'")
      "(tokenizeInt) Is not '+', '-', part of listNumDigits nor listNumDecimal"

--
tokenizeIntEndOperator :: TokenizerOut -> TokenizerOut
tokenizeIntEndOperator (x1, input1, _) =
  case input1 `tokenize` tokenizeOperator of
    (x2, input2, status2) -> (TokenizedLine [x1, x2], input2, status2)

--
tokenizeIntEndDecSeg :: TokenizerIn -> TokenizerOut -> TokenizerOut
tokenizeIntEndDecSeg input (TokenizedInt 0 co1, input1, _) =
  case (shiftedTokenizerIn input1) `tokenize` tokenizeDec of
    (TokenizedFloat x2 _, input2, status2)
      | (headTokenizerIn input) == '-'  -> (TokenizedFloat
        (0 - x2) co1, input2, status2)
      | otherwise                       -> (TokenizedFloat
        x2 co1, input2, status2)
    (x2, input2, status2)                     -> (x2, input2, status2)
tokenizeIntEndDecSeg input (TokenizedInt x1 co1, input1, _) =
  case (shiftedTokenizerIn input1) `tokenize` tokenizeDec of
    (TokenizedFloat x2 _, input2, status2)
      | (headTokenizerIn input) == '-'  -> (TokenizedFloat
        ((fromIntegral x1 :: Float) + (0 - x2)) co1, input2, status2)
      | otherwise                       -> (TokenizedFloat
        ((fromIntegral x1 :: Float) + x2) co1, input2, status2)
    (x2, input2, status2)                     -> (x2, input2, status2)
tokenizeIntEndDecSeg _ (x1, input1, status1) = (x1, input1, status1)

-- | Handles cases where the number is a float
tokenizeIntEndDec :: TokenizerIn -> TokenizerOut -> TokenizerOut
tokenizeIntEndDec input output = case tokenizeIntEndDecSeg input output of
  (x1, input1, status1)
    | (tokenizeIntIfErrorToOperator (x1, input, status1)) ->
      input `tokenize` tokenizeOperator
    | (headTokenizerIn input1) `elem` listOperators       ->
      tokenizeIntEndOperator (x1, input1, status1)
    | otherwise -> (x1, input1, status1)

tokenizeIntIfErrorToOperator :: TokenizerOut -> Bool
tokenizeIntIfErrorToOperator (_, input1, status1) =
  (isParserStatusError status1) &&
    (headTokenizerIn input1) `elem` listOperators

-- | Handles ints and floats, like 7, -42, -1.6180 or .31415
tokenizeInt :: Tokenizer
tokenizeInt = Tokenizer $ \input ->
  case tokenizeIntSeg input (headTokenizerIn input) of
    (x1, input1, status1)
      | (tokenizeIntIfErrorToOperator (x1, input, status1)) ->
        input `tokenize` tokenizeOperator
      | (headTokenizerIn input1) `elem` listOperators       ->
        tokenizeIntEndOperator (x1, input1, status1)
      | (headTokenizerIn input1) `elem` listNumDecimal      ->
        tokenizeIntEndDec input (x1, input1, status1)
      | otherwise -> (x1, input1, status1)
