{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parsing
-}

module Parsing (
  parsingToTokenList,
  parsingToTokenTree
) where

import ParserStatus (
  ParserStatus,
  createParserStatusErrorSimple,
  isParserStatusError
  )

import Parsing.Tokenizer (
  TokenizedAny
  )

import Parsing.Tokenizer.ListOfString (
  tokenizeListOfString
  )

import Parsing.Tokenizer.ListToTree (
  tokenizeListToTree
  )

-- TOKENIZER

--
parsingToTokenList :: [String] -> (Maybe [TokenizedAny], ParserStatus)
parsingToTokenList input = tokenizeListOfString input

--
parsingToTokenTree :: [String] -> (Maybe [TokenizedAny], ParserStatus)
parsingToTokenTree input = case parsingToTokenList input of
  (output, status)
    | isParserStatusError status  -> (output, status)
    | otherwise                   -> case output of
      Nothing                     -> (Nothing, createParserStatusErrorSimple
        "Invalid output"
        "(parsingToTokenTree) parsingToTokenList returned Nothing")
      Just x                      -> tokenizeListToTree x
