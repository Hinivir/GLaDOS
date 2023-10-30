{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parsing/Tokenizer/ListOfString
-}

module Parsing.Tokenizer.ListOfString (
  tokenizeListOfString
) where

import ParserStatus (
  ParserStatus,
  isParserStatusError
  )

import Parsing.Tokenizer (
  TokenizedAny(..)
  )

import Parsing.Tokenizer.Status (
  Tokenizer(..),
  TokenizerOut,
  TokenizerIn,
  createTokenizerOutError,
  createTokenizerOutOK,
  createTokenizerOutOKForce,
  hasTokenizerInEnded,
  headOfShiftedTokenizerIn,
  headTokenizerIn,
  listEmpty,
  listLiteral,
  listNumStart,
  listSymbolsStart,
  listUnique,
  shiftedTokenizerIn,
  signTokenized,
  tokenize
  )

import Parsing.Tokenizer.ListOfString.Int (
  tokenizeInt
  )

import Parsing.Tokenizer.ListOfString.String (
  tokenizeString
  )

-- TOKENIZE --

--
tokenizeLiteralSegChain ::
  TokenizerIn -> Char -> TokenizerIn -> Char -> TokenizerOut -> TokenizerOut
tokenizeLiteralSegChain _ _ input c (TokenizedLiteral x _, input2, status)
  | isParserStatusError status =
    createTokenizerOutOKForce (TokenizedLiteral (c : x) (signTokenized input)) input2
  | otherwise =
    createTokenizerOutOKForce (TokenizedLiteral (c : x) (signTokenized input)) input2
tokenizeLiteralSegChain _ _ input _ (x, input2, status)
  | isParserStatusError status = (x, input2, status)
  | otherwise =
    createTokenizerOutError input "Invalid output"
      "(tokenizeLiteral) tokenizeLiteral didn't return TokenizedLiteral"


--
tokenizeLiteralSeg ::
  TokenizerIn -> Char -> TokenizerIn -> Char -> TokenizerOut
tokenizeLiteralSeg start mark input c
  | mark == c                 =
    createTokenizerOutOK (TokenizedLiteral "" (signTokenized input)) input
  | hasTokenizerInEnded input =
    createTokenizerOutError start "Unclosed mark" "(tokenizeLiteral)"
  | otherwise                 =
    tokenizeLiteralSegChain start mark input c
      (tokenizeLiteralSeg start mark (shiftedTokenizerIn input)
      (headOfShiftedTokenizerIn input))

--
tokenizeLiteralRedirect :: TokenizerIn -> Char -> TokenizerOut
tokenizeLiteralRedirect start mark =
  case shiftedTokenizerIn start of
    input -> tokenizeLiteralSeg start mark input (headTokenizerIn input)

-- | Handles literal strings
tokenizeLiteral :: Tokenizer
tokenizeLiteral = Tokenizer $ \input ->
  case headTokenizerIn input of
    mark -> tokenizeLiteralRedirect input mark

-- | Handles 'Unique' characters, like parentheses
tokenizeUnique :: Tokenizer
tokenizeUnique = Tokenizer $ \input ->
  createTokenizerOutOK
    (TokenizedChar (headTokenizerIn input) (signTokenized input)) input

--
tokenizeAnyOtherwise :: TokenizerIn -> Char -> TokenizerOut
tokenizeAnyOtherwise input c
  | hasTokenizerInEnded input   =
    createTokenizerOutOK TokenizedUndefined input
  | otherwise                   =
    createTokenizerOutError input ("Unreconized Symbol '" ++ [c] ++ "'")
      ("(tokenizeAny) Is not part of listUnique, listLiteral, listNumStart," ++
      " listSymbolsStart nor listEmpty")

--
tokenizeAnySeg :: TokenizerIn -> Char -> TokenizerOut
tokenizeAnySeg input c
  | hasTokenizerInEnded input   = createTokenizerOutOK TokenizedUndefined input
  | c `elem` listUnique         = input `tokenize` tokenizeUnique
  | c `elem` listLiteral        = input `tokenize` tokenizeLiteral
  | c `elem` listNumStart       = input `tokenize` tokenizeInt
  | c `elem` listSymbolsStart   = input `tokenize` tokenizeString
  | c `elem` listEmpty          =
    (shiftedTokenizerIn input) `tokenize` tokenizeAny
  | otherwise                   = tokenizeAnyOtherwise input c

-- | Handles any chracter, using other tokenize[...] functions
tokenizeAny :: Tokenizer
tokenizeAny = Tokenizer $ \input ->
  tokenizeAnySeg input (headTokenizerIn input)

tokenizeListOfStringSub :: TokenizerOut -> (Maybe [TokenizedAny], ParserStatus)
tokenizeListOfStringSub (TokenizedUndefined, output, outerStatus)
  | isParserStatusError outerStatus  = (Nothing, outerStatus)
  | hasTokenizerInEnded output  = (Just [], outerStatus)
  | otherwise                   = case tokenizeListOfStringIn output of
    (Nothing, innerStatus)   -> (Nothing, innerStatus)
    (Just list, innerStatus) -> (Just list, innerStatus)
tokenizeListOfStringSub (x, output, outerStatus)
  | isParserStatusError outerStatus  = (Nothing, outerStatus)
  | hasTokenizerInEnded output  = (Just [x], outerStatus)
  | otherwise                   = case tokenizeListOfStringIn output of
    (Nothing, innerStatus)   -> (Nothing, innerStatus)
    (Just list, innerStatus) -> (Just (x:list), innerStatus)


--
tokenizeListOfStringSeg :: TokenizerIn -> (Maybe [TokenizedAny], ParserStatus)
tokenizeListOfStringSeg input =
  case (input `tokenize` tokenizeAny) of
    output  -> tokenizeListOfStringSub output

--
tokenizeListOfStringIn :: TokenizerIn -> (Maybe [TokenizedAny], ParserStatus)
tokenizeListOfStringIn input = tokenizeListOfStringSeg input

--
tokenizeListOfString :: [String] -> (Maybe [TokenizedAny], ParserStatus)
tokenizeListOfString input = tokenizeListOfStringSeg (input, 1, 1)
