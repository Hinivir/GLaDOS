{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- parserData
-}

--ParserAny (ParserChar, ParserInt, ParserString)
module ParserData (
    ParserAny (ParserChar, ParserInt, ParserString)
) where

data ParserAny = ParserChar Char | ParserInt Int | ParserString String
  deriving (Eq, Show)
