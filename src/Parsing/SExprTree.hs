{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parsing/SExprTree
-}

module Parsing.SExprTree (
  SExpr(..)
) where

-- SEXPR --

-- | The SExpr data type represents an S-Expression.
data SExpr = SExprUndefined
  | SExprBool Bool (Int, Int)
  | SExprInt Int (Int, Int)
  | SExprFloat Float (Int, Int)
  | SExprString String (Int, Int)
  | SExprSymbol String (Int, Int)
  | SExprGroup [SExpr] (Int, Int)
  deriving (Eq, Show)
