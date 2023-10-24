{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parsing/Tokenizer/ListOfString/String
-}

module Parsing.Tokenizer.ListOfString.String (
  tokenizeString
) where

import ParserStatus (
  ParserStatus,
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
  listSymbols,
  shiftedTokenizerIn,
  signTokenized,
  tokenize
  )

import Parsing.Tokenizer (
  TokenizedAny(TokenizedString)
  )

--
tokenizeStringSegChain :: TokenizerIn -> Char -> TokenizerOut -> TokenizerOut
tokenizeStringSegChain input c ((TokenizedString x _), input2, status)
  | isParserStatusError status  =
    createTokenizerOutOKForce
      (TokenizedString [c] (signTokenized input)) input2
  | otherwise                   =
    createTokenizerOutOKForce
      (TokenizedString (c:x) (signTokenized input)) input2
tokenizeStringSegChain input c (x, input2, status)
  | isParserStatusError status  = (x, input2, status)
  | otherwise                   =
    createTokenizerOutError input "Invalid output"
      "(tokenizeString) tokenizeString didn't return TokenizedString"

--
tokenizeStringSeg :: TokenizerIn -> Char -> TokenizerOut
tokenizeStringSeg input c
  | c `elem` listSymbols  =
    tokenizeStringSegChain input c
      ((shiftedTokenizerIn input) `tokenize` tokenizeString)
  | otherwise             =
    (createTokenizerOutError input ("Unreconized Symbol '" ++ [c] ++ "'")
      "(tokenizeString) Is not part of listSymbols")
      `errorContent` (TokenizedString "" (signTokenized input))

-- | Handles regular strings, like symbols or simple keywords
tokenizeString :: Tokenizer
tokenizeString = Tokenizer $ \input ->
  tokenizeStringSeg input (headTokenizerIn input)
