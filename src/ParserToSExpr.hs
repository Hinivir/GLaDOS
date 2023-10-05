{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- parserToSExpr
-}

module ParserToSExpr (
  runParserToSExpr,
  parserToSExprInt,
  parserToSExprChar,
  parserToSExprString,
  parserToSExprList,
  parserToSExpr
) where

import Parser (ParserAny(ParserInt, ParserChar, ParserString))
import SExpr (SExpr(..))

data ParserToSExpr =
  ParserToSExpr
    { runParserToSExpr :: [ParserAny] -> Maybe (SExpr, [ParserAny])
    }

parserToSExprInt :: ParserToSExpr
parserToSExprInt =
  ParserToSExpr $ \input ->
    case input of
      ((ParserInt int):t) -> Just (SInt int, t)
      _                   -> Nothing

parserToSExprChar :: ParserToSExpr
parserToSExprChar =
  ParserToSExpr $ \input ->
    case input of
      ((ParserChar '('):t) -> runParserToSExpr parserToSExprList t
      _       -> Nothing

parserToSExprString :: ParserToSExpr
parserToSExprString =
  ParserToSExpr $ \input ->
    case input of
      ((ParserString "True"):t)   -> Just (SBool True, t)
      ((ParserString "False"):t)  -> Just (SBool False, t)
      ((ParserString string):t)   -> Just (SSym string, t)
      _                           -> Nothing

parserToSExprList :: ParserToSExpr
parserToSExprList =
  ParserToSExpr $ \input -> case input of
    []                    -> Nothing
    ((ParserChar ')'):t)  -> Just (SList [], t)
    _                     -> case runParserToSExpr parserToSExprAny input of
      Nothing                 -> Nothing
      Just (out2, in1)  -> case runParserToSExpr parserToSExprList in1 of
        Just (SList out3, in3)  -> Just (SList (out2:out3), in3)
        _                             -> Nothing

parserToSExprAny :: ParserToSExpr
parserToSExprAny =
  ParserToSExpr $ \input -> case input of
    []    -> Nothing
    (h:_) -> case h of
      (ParserInt _)     -> runParserToSExpr parserToSExprInt input
      (ParserChar _)    -> runParserToSExpr parserToSExprChar input
      (ParserString _)  -> runParserToSExpr parserToSExprString input
--      _                 -> Nothing

parserToSExpr :: Maybe [ParserAny] -> Maybe SExpr
parserToSExpr Nothing = Nothing
parserToSExpr (Just list) =
  case runParserToSExprList (list ++ [(ParserChar ')')]) of
    Nothing           -> Nothing
    Just (output, _)  -> Just output
  where
    runParserToSExprList = runParserToSExpr parserToSExprList
