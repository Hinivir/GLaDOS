{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- SExpr
-}

{-|
Module      : SExpr
Description : Defines the SExpr data type and provides functions to work with S-Expressions.
-}
module SExpr (
  -- * Data Types
  SExpr (..),
  -- * Functions
  getSymbol,
  getInteger,
  getList,
  printTree
) where

import Data.List (intercalate)

-- | The SExpr data type represents an S-Expression.
data SExpr
  = -- | A symbol.
    SSym String
  | -- | An integer.
    SInt Integer
  | -- | A list of S-Expressions.
    SList [SExpr]
  | -- | A Boolean
    SBool Bool
  deriving (Show, Eq)

-- | Returns the symbol value of an S-Expression if it is a symbol.
--
-- Returns 'Nothing' if the S-Expression is not a symbol.
getSymbol :: SExpr -> Maybe String
getSymbol (SSym x) = Just x
getSymbol _ = Nothing

-- | Returns the integer value of an S-Expression if it is an integer.
--
-- Returns 'Nothing' if the S-Expression is not an integer.
getInteger :: SExpr -> Maybe Integer
getInteger (SInt n) = Just n
getInteger _ = Nothing

-- | Returns the list of S-Expressions if the S-Expression is a list.
--
-- Returns 'Nothing' if the S-Expression is not a list.
getList :: SExpr -> Maybe [SExpr]
getList (SList lst) = Just lst
getList _ = Nothing

-- | Returns a string representation of an S-Expression.
printTree :: SExpr -> String
printTree (SSym x) = "a Symbol '" ++ x ++ "'"
printTree (SInt x) = "a Number " ++ show x
printTree (SList x) = "a List with (" ++ printNestedList x

-- | Returns a string representation of a nested list of S-Expressions.
printNestedList :: [SExpr] -> String
printNestedList [SSym x] = "a Symbol '" ++ x ++ "'"
printNestedList [SInt x] = "a Number " ++ show x
printNestedList [SList x] = printNestedList x
printNestedList xs = intercalate ", " (map printTree xs) ++ ")"
