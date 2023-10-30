{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parsing
-}

module Parsing (
  parsingToTokenList,
  parsingToTokenTree,
  parsingToLDataTree,
  parsingToInstruct
) where

import ParserStatus (
  ParserStatus,
  createParserStatusErrorSimple,
  isParserStatusError
  )

import Parsing.LDataTree (
  LData
  )

import Parsing.Instruct (
  Instructions,
  Env
  )

import Parsing.Instruct.LDataToInstruct (
  convertLDataToInstruct
  )

import Parsing.LDataTree.TreeToLData (
  expressTokenizedTree
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

parsingToLDataTree :: [String] -> (Maybe [LData], ParserStatus)
parsingToLDataTree input = case parsingToTokenTree input of
  (output, status)
    | isParserStatusError status  -> (Nothing, status)
    | otherwise                   -> case output of
      Nothing                     -> (Nothing, createParserStatusErrorSimple
        "Invalid output"
        "(parsingToLDataTree) parsingToTokenTree returned Nothing")
      Just x                      -> expressTokenizedTree x

parsingToInstruct :: [String] -> (Maybe Instructions, Env, ParserStatus)
parsingToInstruct input = case parsingToLDataTree input of
  (output, status)
    | isParserStatusError status  -> (Nothing, [], status)
    | otherwise                   -> case output of
      Nothing                     -> (Nothing, [], createParserStatusErrorSimple
        "Invalid output"
        "(parsingToInstruct) parsingToLDataTree returned Nothing")
      Just x                      -> convertLDataToInstruct x []
