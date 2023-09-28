{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Ast
-}

{-|
Module      : Ast
Description : Defines the Ast data type and provides functions to convert SExpr to Ast and evaluate Ast.
-}
module Ast (
  -- * Data Types
  Ast (..),
  -- * Functions
  sexprToAst,
  evalAdd,
  evalSub,
  evalMul,
  evalDiv,
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

-- addition
evalAdd :: Ast -> Ast -> Maybe Ast
evalAdd (Value (SInt x)) (Value (SInt y)) = Just (Value (SInt (x + y)))
evalAdd _ _ = Nothing

-- soustraction
evalSub :: Ast -> Ast -> Maybe Ast
evalSub (Value (SInt x)) (Value (SInt y)) = Just (Value (SInt (x - y)))
evalSub _ _ = Nothing

-- multiplication
evalMul :: Ast -> Ast -> Maybe Ast
evalMul (Value (SInt x)) (Value (SInt y)) = Just (Value (SInt (x * y)))
evalMul _ _ = Nothing

-- division
evalDiv :: Ast -> Ast -> Maybe Ast
evalDiv (Value (SInt _)) (Value (SInt 0)) = Nothing
evalDiv (Value (SInt x)) (Value (SInt y)) = Just (Value (SInt (x `div` y)))
evalDiv _ _ = Nothing

evalAst :: Ast -> Maybe Ast
evalAst (Value v) = Just (Value v)
evalAst (Define _ _) = Nothing -- impossible d'évaluer une définition
evalAst (Call "+" [a, b]) = case (evalAst a, evalAst b) of
  (Just x, Just y) -> evalAdd x y
  _ -> Nothing
evalAst (Call "-" [a, b]) = case (evalAst a, evalAst b) of
  (Just x, Just y) -> evalSub x y
  _ -> Nothing
evalAst (Call "*" [a, b]) = case (evalAst a, evalAst b) of
  (Just x, Just y) -> evalMul x y
  _ -> Nothing
evalAst (Call "/" [a, b]) = case (evalAst a, evalAst b) of
  (Just x, Just y) -> evalDiv x y
  _ -> Nothing
evalAst (Call _ _) = Nothing
