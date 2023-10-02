{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- parserToSExpr
-}

module ParserToSExpr (
    parserToSExpr
) where

import Parser (Parser)
import SExpr (SExpr)

parserToSExpr :: Parser Char -> Maybe SExpr
parserToSExpr _ = Nothing