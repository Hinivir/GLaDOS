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
  evalMod,
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

-- modulo
evalMod :: Ast -> Ast -> Maybe Ast
evalMod (Value (SInt _)) (Value (SInt 0)) = Nothing
evalMod (Value (SInt x)) (Value (SInt y)) = Just (Value (SInt (x `mod` y)))
evalMod _ _ = Nothing

evalAst :: Ast -> Maybe Ast
evalAst (Value v) = Just (Value v)
evalAst (Define _ _) = Nothing
evalAst (Call "+" [a, b]) = evalBinOp evalAdd a b
evalAst (Call "-" [a, b]) = evalBinOp evalSub a b
evalAst (Call "*" [a, b]) = evalBinOp evalMul a b
evalAst (Call "div" [a, b]) = evalBinOp evalDiv a b
evalAst (Call "mod" [a, b]) = evalBinOp evalMod a b
evalAst (Call ">" [a, b])
  | Just (Value (SInt x)) <- evalAst a
  , Just (Value (SInt y)) <- evalAst b =
    Just (Value (SBool (x > y)))
  | otherwise = Nothing
evalAst  (Call "<" [a, b])
  | Just (Value (SInt x)) <- evalAst a
  , Just (Value (SInt y)) <- evalAst b =
    Just (Value (SBool (x < y)))
  | otherwise = Nothing
evalAst (Call "eq?" [a, b])
  | Just (Value (SInt x)) <- evalAst a
  , Just (Value (SInt y)) <- evalAst b =
    Just (Value (SBool (x == y)))
  | otherwise = Nothing
evalAst (Call "if" [condExpr, trueExpr, falseExpr]) =
  evalIf condExpr trueExpr falseExpr
evalAst (Call _ _) = Nothing

evalBinOp :: (Ast -> Ast -> Maybe Ast) -> Ast -> Ast -> Maybe Ast
evalBinOp op a b =
  case (evalAst a, evalAst b) of
    (Just x, Just y) -> op x y
    _                -> Nothing

evalIf :: Ast -> Ast -> Ast -> Maybe Ast
evalIf condExpr trueExpr falseExpr =
  case (evalAst condExpr, evalAst trueExpr, evalAst falseExpr) of
    (Just (Value (SBool condition)), Just x, Just y) ->
      if condition
        then Just x
        else Just y
    _ -> Nothing
