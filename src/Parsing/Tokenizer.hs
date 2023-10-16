{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parsing/Tokenizer
-}

module Parsing.Tokenizer (
  TokenizedAny(..)
) where

-- TOKENIZED --

--
data TokenizedAny = TokenizedUndefined
  | TokenizedChar Char (Int, Int)     | TokenizedInt Int (Int, Int)
  | TokenizedString String (Int, Int)
  | TokenizedLiteral String (Int, Int)
  deriving (Eq, Show)
