{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parsing/Tokenizer/ListOfString/Dec
-}

module Parsing.Tokenizer.ListOfString.Dec (
  tokenizeDec
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
  listNumDigits,
  shiftedTokenizerIn,
  signTokenized
  )

import Parsing.Tokenizer (
  TokenizedAny(TokenizedFloat)
  )

--
charToDec :: Char -> Float
charToDec '1' = 0.1
charToDec '2' = 0.2
charToDec '3' = 0.3
charToDec '4' = 0.4
charToDec '5' = 0.5
charToDec '6' = 0.6
charToDec '7' = 0.7
charToDec '8' = 0.8
charToDec '9' = 0.9
charToDec _ = 0

--
tokenizeUDecSegChainOnError :: TokenizerIn -> Char -> TokenizerOut ->
  TokenizerOut
tokenizeUDecSegChainOnError input c ((TokenizedFloat _ _), input2, _)
  | (headOfShiftedTokenizerIn input) `elem` listSymbols &&
    not ((headOfShiftedTokenizerIn input) `elem` listOperators) =
      createTokenizerOutError (shiftedTokenizerIn input)
        ("Unreconized Symbol '" ++ [headOfShiftedTokenizerIn input] ++ "'")
        "(tokenizeDec) Is part of listSymbols"
  | otherwise = (createTokenizerOutOKForce
    (TokenizedFloat (charToDec c) (signTokenized input)) input2)
tokenizeUDecSegChainOnError input c (_, input2, _) =
    (createTokenizerOutOKForce
      (TokenizedFloat (charToDec c) (signTokenized input)) input2)

--
tokenizeUDecSegChain :: TokenizerIn -> Char -> TokenizerOut -> TokenizerOut
tokenizeUDecSegChain input c ((TokenizedFloat x coor), input2, status)
  | isParserStatusError status  =
    tokenizeUDecSegChainOnError input c
      ((TokenizedFloat x coor), input2, status)
  | otherwise                   =
    createTokenizerOutOKForce
      (TokenizedFloat ((charToDec c) + x / 10) (signTokenized input)) input2
tokenizeUDecSegChain input _ (x, input2, status)
  | isParserStatusError status  = (x, input2, status)
  | otherwise                   =
    createTokenizerOutError input "Invalid output"
      "(tokenizeDec) tokenizeDec didn't return TokenizedFloat"

--
tokenizeUDecSeg :: TokenizerIn -> Char -> TokenizerOut
tokenizeUDecSeg input c
  | c `elem` listNumDigits  =
    tokenizeUDecSegChain input c (tokenizeUDecSeg
      (shiftedTokenizerIn input) (headOfShiftedTokenizerIn input))
  | otherwise               =
    (createTokenizerOutError input ("Unreconized Symbol '" ++ [c] ++ "'")
      "(tokenizeDec) Is not part of listNumDigits")
      `errorContent` (TokenizedFloat 0 (signTokenized input))

--
tokenizeDecSeg :: TokenizerIn -> Char -> TokenizerOut
tokenizeDecSeg input c
  | c `elem` listNumDigits  = case tokenizeUDecSeg input c of
    output  -> output
  | otherwise               =
    createTokenizerOutError input ("Unreconized Symbol '" ++ [c] ++ "'")
      "(tokenizeDec) Is not part of listNumDigits"

-- | Handles decimal parts
tokenizeDec :: Tokenizer
tokenizeDec = Tokenizer $ \input ->
  tokenizeDecSeg input (headTokenizerIn input)
