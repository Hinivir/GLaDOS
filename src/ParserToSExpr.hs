{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- parserToSExpr
-}

module ParserToSExpr (
  runParserToSexpr,
  parserToSExprInt,
  parserToSExprChar,
  parserToSExprList,
  parserToSExpr
) where

import Parser (ParserAny(ParserInt, ParserChar))
import SExpr (SExpr(..))

data ParserToSexpr =
  ParserToSexpr
    { runParserToSexpr :: [ParserAny] -> Maybe (SExpr, [ParserAny])
    }

parserToSExprInt :: ParserToSexpr
parserToSExprInt =
  ParserToSexpr $ \input ->
    case input of
      ((ParserInt int):t) -> Just (SInt int, t)
      _                   -> Nothing

parserToSExprChar :: ParserToSexpr
parserToSExprChar =
  ParserToSexpr $ \input ->
    case input of
      ((ParserChar '('):t) -> runParserToSexpr parserToSExprList t
      _       -> Nothing

parserToSExprList :: ParserToSexpr
parserToSExprList =
  ParserToSexpr $ \input ->
    Nothing

--parserToSExprList =

parserToSExprParserAny :: [ParserAny] -> Maybe SExpr
parserToSExprParserAny [] = Nothing
parserToSExprParserAny ((ParserInt int):t) = Just (SInt int)
parserToSExprParserAny _ = Nothing

parserToSExpr :: Maybe [ParserAny] -> Maybe SExpr
parserToSExpr Nothing = Nothing
parserToSExpr (Just list) = parserToSExprParserAny list
