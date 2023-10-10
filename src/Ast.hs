{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Ast
-}

module Ast
  -- * Data Types
  ( Ast(..)
  , Env
  -- * Functions
  , sexprToAst
  , evalAdd
  , evalSub
  , evalMul
  , evalDiv
  , evalMod
  , evalAst
  ) where

import SExpr (SExpr (..))

import qualified Data.Map as Map

data Ast
  = Define String Ast
  | Call String [Ast]
  | Value SExpr
  | Lambda ([Ast] -> Either String Ast)

type Env = Map.Map String Ast

sexprToAst :: SExpr -> Either String Ast
sexprToAst (SList [SSym "define", SSym var, expr]) = do
  exprAst <- sexprToAst expr
  Right (Define var exprAst)
sexprToAst (SList [x]) = sexprToAst x
sexprToAst (SList (SSym func:args)) =
  case mapM sexprToAst args of
    Left str   -> Left str
    Right asts -> Right (Call func asts)
sexprToAst (SInt x) = Right (Value (SInt x))
sexprToAst (SBool x) = Right (Value (SBool x))
sexprToAst (SSym "#t") = Right (Value (SBool True))
sexprToAst (SSym "#f") = Right (Value (SBool False))
sexprToAst (SSym x) = Right (Value (SSym x))
sexprToAst (SList []) = Left "error empty list"
sexprToAst (SList nestedExprs) = do
  nestedAsts <- mapM sexprToAst nestedExprs
  Right (Call "nested" nestedAsts)

-- addition
evalAdd :: Ast -> Ast -> Maybe Ast
evalAdd (Value (SInt x)) (Value (SInt y)) = Just (Value (SInt (x + y)))
evalAdd _ _                               = Nothing

-- soustraction
evalSub :: Ast -> Ast -> Maybe Ast
evalSub (Value (SInt x)) (Value (SInt y)) = Just (Value (SInt (x - y)))
evalSub _ _                               = Nothing

-- multiplication
evalMul :: Ast -> Ast -> Maybe Ast
evalMul (Value (SInt x)) (Value (SInt y)) = Just (Value (SInt (x * y)))
evalMul _ _                               = Nothing

-- division
evalDiv :: Ast -> Ast -> Maybe Ast
evalDiv (Value (SInt _)) (Value (SInt 0)) = Nothing
evalDiv (Value (SInt x)) (Value (SInt y)) = Just (Value (SInt (x `div` y)))
evalDiv _ _                               = Nothing

-- modulo
evalMod :: Ast -> Ast -> Maybe Ast
evalMod (Value (SInt _)) (Value (SInt 0)) = Nothing
evalMod (Value (SInt x)) (Value (SInt y)) = Just (Value (SInt (x `mod` y)))
evalMod _ _                               = Nothing

evalAst :: Ast -> Env -> Maybe (Ast, Env)
evalAst (Value v) env = Just (Value v, env)
evalAst (Define var expr) env = do
  (exprValue, newEnv) <- evalAst expr env
  Just (Define var exprValue, newEnv)
evalAst (Call "+" [a, b]) env = evalBinOp evalAdd a b env
evalAst (Call "-" [a, b]) env = evalBinOp evalSub a b env
evalAst (Call "*" [a, b]) env = evalBinOp evalMul a b env
evalAst (Call "div" [a, b]) env = evalBinOp evalDiv a b env
evalAst (Call "mod" [a, b]) env = evalBinOp evalMod a b env
evalAst (Call ">" [a, b]) env = evalComparison (>) a b env
evalAst (Call "<" [a, b]) env = evalComparison (<) a b env
evalAst (Call "eq?" [a, b]) env = evalComparison (==) a b env
evalAst (Call "if" [condExpr, trueExpr, falseExpr]) env =
  evalIf condExpr trueExpr falseExpr env
evalAst (Lambda x) env = Just (Lambda x, env)
evalAst (Call func args) env = do
  funcAst <- lookupFunction func env
  evalFunction funcAst args env
  where
    lookupFunction :: String -> Env -> Maybe Ast
    lookupFunction func env = Map.lookup func env

evalBinOp :: (Ast -> Ast -> Maybe Ast) -> Ast -> Ast -> Env -> Maybe (Ast, Env)
evalBinOp op a b env = do
  (aValue, env1) <- evalAst a env
  (bValue, env2) <- evalAst b env1
  result <- op aValue bValue
  Just (result, env2)

evalComparison :: (Int -> Int -> Bool) -> Ast -> Ast -> Env -> Maybe (Ast, Env)
evalComparison compOp a b env = do
  (aValue, env1) <- evalAst a env
  (bValue, env2) <- evalAst b env1
  case (aValue, bValue) of
    (Value (SInt x), Value (SInt y)) -> Just (Value (SBool (compOp x y)), env2)
    _                                -> Nothing

evalIf :: Ast -> Ast -> Ast -> Env -> Maybe (Ast, Env)
evalIf condExpr trueExpr falseExpr env = do
  (condValue, env1) <- evalAst condExpr env
  case condValue of
    Value (SBool condition) ->
      if condition
        then evalAst trueExpr env1
        else evalAst falseExpr env1
    _ -> Nothing

evalFunction :: Ast -> [Ast] -> Env -> Maybe (Ast, Env)
evalFunction (Lambda func) args env =
  case func args of
    Left errMsg  -> Nothing
    Right result -> evalAst result env
evalFunction _ _ _ = Nothing
