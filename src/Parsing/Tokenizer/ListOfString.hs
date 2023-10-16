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
  createParserStatusError,
  createParserStatusErrorGeneric,
  createParserStatusOk,
  isParserStatusError
  )

import Parsing.Tokenizer (
  TokenizedAny(..)
  )

-- TOKENIZER

type TokenizerIn = ([String], Int, Int)
type TokenizerOut = (TokenizedAny, TokenizerIn, ParserStatus)

--
data Tokenizer =
  Tokenizer
    { runTokenizer :: TokenizerIn -> TokenizerOut }

--
headTokenizerIn :: TokenizerIn -> Char
headTokenizerIn ([], _, _) = ' '
headTokenizerIn (([]:_), _, _) = '\n'
headTokenizerIn (((c:_):_), ln, col) = c

--
shiftedTokenizerIn :: TokenizerIn -> TokenizerIn
shiftedTokenizerIn ([], ln, col) = ([], ln, col)
shiftedTokenizerIn (([]:rem), _, col) = (rem, 1, col + 1)
shiftedTokenizerIn (((_:[]):rem), _, col) = (([]:rem), 1, col)
shiftedTokenizerIn (((_:t):rem), ln, col) = ((t:rem), ln + 1, col)

--
headOfShitedTokenizerIn :: TokenizerIn -> Char
headOfShitedTokenizerIn input = headTokenizerIn (shiftedTokenizerIn input)

--
hasTokenizerInEnded :: TokenizerIn -> Bool
hasTokenizerInEnded ([], _, _) = True
hasTokenizerInEnded (([]:[]), _, _) = True
hasTokenizerInEnded _ = False

--
signTokenized :: TokenizerIn -> (Int, Int)
signTokenized (_, ln, col) = (ln, col)

--
createTokenizerOutOKForce :: TokenizedAny -> TokenizerIn -> TokenizerOut
createTokenizerOutOKForce a b = (a, b, createParserStatusOk)

--
createTokenizerOutOK :: TokenizedAny -> TokenizerIn -> TokenizerOut
createTokenizerOutOK a b = createTokenizerOutOKForce a (shiftedTokenizerIn b)

--
createTokenizerOutError :: TokenizerIn -> String -> String -> TokenizerOut
createTokenizerOutError input name info = case input of
  (_, ln, col) ->
    (TokenizedUndefined, input, createParserStatusError name info ln col)

--
errorContent :: (TokenizerOut) -> TokenizedAny -> TokenizerOut
errorContent (_, input, status) content = (content ,input, status)

-- FUNCTIONS --

--
tokenize :: TokenizerIn -> Tokenizer -> (TokenizerOut)
tokenize a f = runTokenizer f a

--
listIntSigns :: [Char]
listIntSigns = "+-"

--
listNumbers :: [Char]
listNumbers = ['0'..'9']

--
listSymbols :: [Char]
listSymbols = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "+-*/%<>=?!#."

--
listUnique :: [Char]
listUnique = "(),"

--
listEmpty :: [Char]
listEmpty = " \t\n"

--
listLiteral :: [Char]
listLiteral = "\"\'"

-- TOKENIZE --

--
tokenizeLiteralSegChain ::
  TokenizerIn -> Char -> TokenizerIn -> Char -> TokenizerOut -> TokenizerOut
tokenizeLiteralSegChain start mark input c
  ((TokenizedLiteral x _), input2, status)
  | isParserStatusError status  =
    createTokenizerOutOKForce
      (TokenizedLiteral [c] (signTokenized input)) input2
  | otherwise                   =
    createTokenizerOutOKForce
      (TokenizedLiteral (c:x) (signTokenized input)) input2
tokenizeLiteralSegChain start mark input c (x, input2, status)
  | isParserStatusError status  = (x, input2, status)
  | otherwise                   =
    createTokenizerOutError input "Invalid output"
      ("(tokenizeLiteral) " ++
      "tokenizeLiteral didn't return TokenizedLiteral")


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
      (headOfShitedTokenizerIn input))

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
      ("(tokenizeAny) Is not part of listUnique, listLiteral, listSymbols " ++
      "nor listEmpty")

--
tokenizeAnySeg :: TokenizerIn -> Char -> TokenizerOut
tokenizeAnySeg input c
  | c `elem` listUnique         = input `tokenize` tokenizeUnique
  | c `elem` listLiteral        = input `tokenize` tokenizeLiteral
  | c `elem` listSymbols        = input `tokenize` tokenizeString
  | c `elem` listEmpty          =
    (shiftedTokenizerIn input) `tokenize` tokenizeAny
  | otherwise                   = tokenizeAnyOtherwise input c

-- | Handles any chracter, using other tokenize[...] functions
tokenizeAny :: Tokenizer
tokenizeAny = Tokenizer $ \input ->
  tokenizeAnySeg input (headTokenizerIn input)

--
tokenizeListOfStringSeg :: TokenizerIn -> (Maybe [TokenizedAny], ParserStatus)
tokenizeListOfStringSeg input =
  case (input `tokenize` tokenizeAny) of
    (x, output, status)
      | isParserStatusError status  -> (Nothing, status)
      | hasTokenizerInEnded output  -> (Just [x], status)
      | otherwise                   -> case tokenizeListOfStringIn output of
        (Nothing, status)           -> (Nothing, status)
        (Just list, status)         -> (Just (x:list), status)

--
tokenizeListOfStringIn :: TokenizerIn -> (Maybe [TokenizedAny], ParserStatus)
tokenizeListOfStringIn input = tokenizeListOfStringSeg input

--
tokenizeListOfString :: [String] -> (Maybe [TokenizedAny], ParserStatus)
tokenizeListOfString input = tokenizeListOfStringSeg (input, 1, 1)
