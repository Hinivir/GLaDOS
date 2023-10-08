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

-- Erreur Parser ?
import ParserData
import SExpr (SExpr(..))

-------------------------------------------------
import Ast()
-------------------------------------------------

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
      ((ParserChar '('):t)  -> runParserToSExpr parserToSExprList t
      _                     -> Nothing

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
      Nothing           -> Nothing
      Just (out2, in1)  -> case runParserToSExpr parserToSExprList in1 of
        Just (SList out3, in3)  -> Just (SList (out2:out3), in3)
        _                       -> Nothing

parserToSExprAny :: ParserToSExpr
parserToSExprAny =
  ParserToSExpr $ \input -> case input of
    []    -> Nothing
    (h:_) -> case h of
      (ParserInt _)     -> runParserToSExpr parserToSExprInt input
      (ParserChar _)    -> runParserToSExpr parserToSExprChar input
      (ParserString _)  -> runParserToSExpr parserToSExprString input
--      _                 -> Nothing

-- function parserToSExpr
--
-- It's all in the name of the function (...)
parserToSExpr :: Maybe [ParserAny] -> Maybe SExpr
parserToSExpr Nothing = Nothing --OK
parserToSExpr (Just list) =
  case runParserToSExprList (list ++ [(ParserChar ')')]) of -- Doit terminer par une ')'
    Nothing           -> Nothing --OK
    Just (output, _)  -> Just output --OK
                         -- sexprToAst (Just output)
  where
    runParserToSExprList = runParserToSExpr parserToSExprList

-------------------------------------------------
{-
Fichier => Ast.hs
Fonction => sexprToAst :: SExpr -> Maybe Ast

-}
-------------------------------------------------
