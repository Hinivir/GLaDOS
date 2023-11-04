{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parsing/Tokenizer/ListOfString/Operator
-}

module Parsing.Tokenizer.ListOfString.Operator (
  tokenizeOperator
) where

import ParserStatus (
  isParserStatusError
  )

import Parsing.Tokenizer.Status (
  Tokenizer(..),
  TokenizerIn,
  TokenizerOut,
  createTokenizerOutError,
  createTokenizerOutOKForce,
  headOfShiftedTokenizerIn,
  headTokenizerIn,
  listOperators,
  listOperatorsComb,
  shiftedTokenizerIn,
  signTokenized
  )

import Parsing.Tokenizer (
  TokenizedAny(TokenizedString)
  )

--
tokenizeOperatorChain :: TokenizerIn -> Char -> String -> TokenizerOut
tokenizeOperatorChain input c str = case tokenizeOperatorSeg
  (shiftedTokenizerIn input) (headOfShiftedTokenizerIn input) (str ++ [c]) of
    (TokenizedString x1 coor1, input1, status1)
      | isParserStatusError status1 -> createTokenizerOutOKForce
        (TokenizedString str (signTokenized input)) input
      | otherwise                   ->
        (TokenizedString x1 coor1, input1, status1)
    (_, _, _) -> createTokenizerOutOKForce
      (TokenizedString str (signTokenized input)) input

--
tokenizeOperatorSeg :: TokenizerIn -> Char -> String -> TokenizerOut
tokenizeOperatorSeg input c []
  | c `elem` listOperators  =
    tokenizeOperatorSeg
      (shiftedTokenizerIn input) (headOfShiftedTokenizerIn input) [c]
  | otherwise               =
    createTokenizerOutError input ("Unreconized operator '" ++ [c] ++ "'")
      "(tokenizeOperator) Is not part of listOperators"
tokenizeOperatorSeg input c str
  | not (c `elem` listOperators) && str `elem` listOperatorsComb  =
    createTokenizerOutOKForce
      (TokenizedString str (signTokenized input)) input
  | not (c `elem` listOperators)                                  =
    createTokenizerOutError input ("Unreconized operator '" ++ str ++ "'")
      "(tokenizeOperator) Is not part of listOperatorsComb"
  | str `elem` listOperatorsComb  = tokenizeOperatorChain input c str
  | otherwise = tokenizeOperatorSeg
    (shiftedTokenizerIn input) (headOfShiftedTokenizerIn input) (str ++ [c])

-- | Handles regular operators, like additions or comparisons
tokenizeOperator :: Tokenizer
tokenizeOperator = Tokenizer $ \input ->
  tokenizeOperatorSeg
    input (headTokenizerIn input) []
