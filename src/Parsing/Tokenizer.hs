{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parsing/Tokenizer
-}

module Parsing.Tokenizer (
  TokenizedAny(..),
  getTokenizerCoordinates
) where

-- TOKENIZER --

--
data TokenizedAny = TokenizedUndefined
  | TokenizedChar Char (Int, Int)
  | TokenizedInt Int (Int, Int)
  | TokenizedFloat Float (Int, Int)
  | TokenizedString String (Int, Int)
  | TokenizedLiteral String (Int, Int)
  | TokenizedList Char [TokenizedAny] (Int, Int)
  | TokenizedLine [TokenizedAny]
  deriving (Eq, Show)

--
getTokenizerCoordinates :: TokenizedAny -> (Int, Int)
getTokenizerCoordinates (TokenizedChar _ coor) = coor
getTokenizerCoordinates (TokenizedInt _ coor) = coor
getTokenizerCoordinates (TokenizedFloat _ coor) = coor
getTokenizerCoordinates (TokenizedString _ coor) = coor
getTokenizerCoordinates (TokenizedLiteral _ coor) = coor
getTokenizerCoordinates (TokenizedList _ _ coor) = coor
getTokenizerCoordinates (TokenizedLine _) = (0, 0)
getTokenizerCoordinates _ = (0, 0)
