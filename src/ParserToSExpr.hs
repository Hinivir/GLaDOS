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
  parserToSExprString,
  parserToSExprList,
  parserToSExpr
) where

import Parser (ParserAny(ParserInt, ParserChar, ParserString))
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

parserToSExprString :: ParserToSexpr
parserToSExprString =
  ParserToSexpr $ \input ->
    case input of
      ((ParserString "True"):t)   -> Just (SBool True, t)
      ((ParserString "False"):t)  -> Just (SBool False, t)
      ((ParserString string):t)   -> Just (SSym string, t)
      _                           -> Nothing

parserToSExprList :: ParserToSexpr
parserToSExprList =
  ParserToSexpr $ \input -> case input of
    []                    -> Nothing
    ((ParserChar ')'):t)  -> Just (SList [], t)
    _                     -> case runParserToSexpr parserToSExprAny input of
      Nothing                 -> Nothing
      Just (out2, in1)  -> case runParserToSexpr parserToSExprList in1 of
        Just (SList out3, in3)  -> Just (SList (out2:out3), in3)
        _                             -> Nothing

parserToSExprAny :: ParserToSexpr
parserToSExprAny =
  ParserToSexpr $ \input -> case input of
    []    -> Nothing
    (h:_) -> case h of
      (ParserInt _)     -> runParserToSexpr parserToSExprInt input
      (ParserChar _)    -> runParserToSexpr parserToSExprChar input
      (ParserString _)  -> runParserToSexpr parserToSExprString input
--      _                 -> Nothing

parserToSExpr :: Maybe [ParserAny] -> Maybe SExpr
parserToSExpr Nothing = Nothing
parserToSExpr (Just list) = case runParserToSexpr parserToSExprList list of
  Nothing           -> Nothing
  Just (output, _)  -> Just output
