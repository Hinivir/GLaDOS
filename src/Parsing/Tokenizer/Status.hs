{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parsing/Tokenizer/Status
-}

module Parsing.Tokenizer.Status (
  Tokenizer(..),
  TokenizerOut,
  TokenizerIn,
  createTokenizerOutError,
  createTokenizerOutOK,
  createTokenizerOutOKForce,
  errorContent,
  hasTokenizerInEnded,
  headOfShiftedTokenizerIn,
  headTokenizerIn,
  isUniquePair,
  listEmpty,
  listLiteral,
  listNumSigns,
  listNumDigits,
  listNumStart,
  listSymbolsStart,
  listSymbols,
  listUnique,
  listUniqueEnd,
  listUniquePair,
  listUniquePairEnd,
  listUniquePairStart,
  listUniqueSnitch,
  shiftedTokenizerIn,
  signTokenized,
  tokenize
) where

import ParserStatus (
  ParserStatus,
  createParserStatusError,
  createParserStatusOk,
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
headTokenizerIn (((c:_):_), _, _) = c

--
shiftedTokenizerIn :: TokenizerIn -> TokenizerIn
shiftedTokenizerIn ([], ln, col) = ([], ln, col)
shiftedTokenizerIn (([]:remaining), _, col) = (remaining, 1, col + 1)
shiftedTokenizerIn (((_:[]):remaining), _, col) = (([]:remaining), 1, col)
shiftedTokenizerIn (((_:t):remaining), ln, col) = ((t:remaining), ln + 1, col)

--
headOfShiftedTokenizerIn :: TokenizerIn -> Char
headOfShiftedTokenizerIn input = headTokenizerIn (shiftedTokenizerIn input)

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
listNumSigns :: [Char]
listNumSigns = "+-"

--
listNumDigits :: [Char]
listNumDigits = ['0'..'9']

--
listNumStart :: [Char]
listNumStart = listNumDigits ++ listNumSigns

--
listSymbolsStart :: [Char]
listSymbolsStart = ['a'..'z'] ++ ['A'..'Z'] ++ "+-*/%<>=?!#."

--
listSymbols :: [Char]
listSymbols = listSymbolsStart ++ ['0'..'9']

--
listUniquePairEnd :: [Char]
listUniquePairEnd = ")]}"

--
listUniquePairStart :: [Char]
listUniquePairStart = "([{"

--
listUniquePair :: [Char]
listUniquePair = listUniquePairStart ++ listUniquePairEnd

-- | Linebreaks
listUniqueEnd :: [Char]
listUniqueEnd = "\n|;"

-- | Snitches are used to ignore linebreaks (listUniqueEnd)
listUniqueSnitch :: [Char]
listUniqueSnitch = "\\"

--
listUnique :: [Char]
listUnique = listUniquePair ++ listUniqueEnd ++ listUniqueSnitch ++ ",:"

--
listEmpty :: [Char]
listEmpty = " \t"

--
listLiteral :: [Char]
listLiteral = "\"\'"

--
isUniquePair :: Char -> Char -> Bool
isUniquePair '(' ')' = True
isUniquePair '[' ']' = True
isUniquePair '{' '}' = True
isUniquePair _ _ = False
