{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- SExpr
-}

{-|
Module      : SExpr
Description : Definition of Types and Functions for S-Expressions

This module defines the 'SExpr' data type, which represents S-Expressions, and provides functions for working with S-Expressions.

== Data Types

- 'SExpr': The primary data type representing S-Expressions. It can be one of the following constructors:
  - 'SSym': Represents a symbol (string).
  - 'SInt': Represents an integer.
  - 'SList': Represents a list of S-Expressions.
  - 'SBool': Represents a boolean value.

== Functions

- 'getSymbol': Retrieves the symbol value from an S-Expression if it is a symbol. Returns 'Nothing' if the S-Expression is not a symbol.

- 'getInteger': Retrieves the integer value from an S-Expression if it is an integer. Returns 'Nothing' if the S-Expression is not an integer.

- 'getList': Retrieves a list of S-Expressions if the S-Expression is a list. Returns 'Nothing' if the S-Expression is not a list.

- 'printTree': Returns a string representation of an S-Expression. It provides a human-readable description of the S-Expression.

- 'printNestedList': Returns a string representation of a nested list of S-Expressions. It is used by 'printTree' to display lists.

== Usage Example

To use this module, you can create S-Expressions and manipulate them using the provided functions:

1. Create S-Expressions using the 'SExpr' constructors ('SSym', 'SInt', 'SList', 'SBool').

2. Use 'getSymbol' to extract symbol values, 'getInteger' to extract integer values, and 'getList' to work with lists.

3. Display S-Expressions with 'printTree' to obtain human-readable representations.

-}

module SExpr
    -- * Data Types
  ( SExpr(..)
    -- * Functions
  , getSymbol
  , getInteger
  , getList
  , printTree
  ) where

import Data.List (intercalate)

-- | The SExpr data type represents an S-Expression.
data SExpr
  = -- | A symbol.
    SSym String
  | -- | An integer.
    SInt Int
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
getInteger :: SExpr -> Maybe Int
getInteger (SInt n) = Just n
getInteger _ = Nothing

-- | Returns the list of S-Expressions if the S-Expression is a list.
--
-- Returns 'Nothing' if the S-Expression is not a list.
getList :: SExpr -> Maybe [SExpr]
getList (SList lst) = Just lst
getList _ = Nothing

-- | Print a tree of S-Expressions.
-- | Returns a string representation of an S-Expression.
printTree :: SExpr -> String
printTree (SSym x) = "a Symbol '" ++ x ++ "'"
printTree (SInt x) = "a Number " ++ show x
printTree (SBool x) = "a Bool " ++ show x
printTree (SList x) = "a List with (" ++ printNestedList x

-- | Print a nested list of S-Expressions.
-- | Returns a string representation of a nested list of S-Expressions.
printNestedList :: [SExpr] -> String
printNestedList [SSym x] = "a Symbol '" ++ x ++ "'"
printNestedList [SInt x] = "a Number " ++ show x
printNestedList [SList x] = printNestedList x
printNestedList xs = intercalate ", " (map printTree xs) ++ ")"
