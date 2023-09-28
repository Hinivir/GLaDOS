{-|
Module      : Ast
Description : Defines the Ast data type and provides functions to convert SExpr to Ast and evaluate Ast.
-}
module Ast (
  -- * Data Types
  Ast (..),
  -- * Functions
  sexprToAst,
  evalAst
) where

import SExpr (SExpr (..))

-- | The Ast data type represents an abstract syntax tree.
data Ast
  = -- | A definition of a variable with a value.
    Define String Ast
  | -- | A function call with a function name and arguments.
    Call String [Ast]
  | -- | A value represented by an SExpr.
    Value SExpr
  deriving (Show, Eq)

-- | Converts an SExpr to an Ast.
--
-- Returns 'Nothing' if the SExpr cannot be converted to an Ast.
sexprToAst :: SExpr -> Maybe Ast
sexprToAst (SList [SSym "define", SSym var, expr]) =
  case sexprToAst expr of
    Just astExpr -> Just (Define var astExpr)
    Nothing -> Nothing
sexprToAst (SList (SSym func : args)) =
  case mapM sexprToAst args of
    Just astArgs -> Just (Call func astArgs)
    Nothing -> Nothing
sexprToAst (SInt n) = Just (Value (SInt n))
sexprToAst (SSym s) = Just (Value (SSym s))
sexprToAst _ = Nothing

-- | Evaluates an Ast and returns the result.
--
-- Returns 'Nothing' if the Ast cannot be evaluated.
evalAst :: Ast -> Maybe Ast
evalAst (Value v) = Just (Value v)
evalAst (Define _ _) = Nothing -- cannot evaluate a definition
evalAst (Call "+" [a, b]) = case (evalAst a, evalAst b) of
  (Just (Value (SInt x)), Just (Value (SInt y))) -> Just (Value (SInt (x + y)))
  _ -> Nothing
evalAst (Call "-" [a, b]) = case (evalAst a, evalAst b) of
  (Just (Value (SInt x)), Just (Value (SInt y))) -> Just (Value (SInt (x - y)))
  _ -> Nothing
evalAst (Call "*" [a, b]) = case (evalAst a, evalAst b) of
  (Just (Value (SInt x)), Just (Value (SInt y))) -> Just (Value (SInt (x * y)))
  _ -> Nothing
evalAst (Call "/" [a, b]) = case (evalAst a, evalAst b) of
  (Just (Value (SInt x)), Just (Value (SInt 0))) -> Nothing -- division by zero
  (Just (Value (SInt x)), Just (Value (SInt y))) -> Just (Value (SInt (x `div` y)))
  _ -> Nothing
evalAst (Call _ _) = Nothing -- cannot evaluate other function calls