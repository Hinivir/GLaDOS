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
  | Symbol String
 deriving (Show, Eq)

type Env = Map.Map String Ast

tryReadVar :: String -> Env -> Either (Ast, Env) String
tryReadVar key m =
  case Map.lookup key m of
    Nothing -> Right $ "Unknown symbol " ++ key
    Just v  -> Left (v, m)

lookupSymbol :: String -> Env -> Either String Ast
lookupSymbol symbol env =
  case Map.lookup symbol env of
    Just value -> Right value
    Nothing    -> Left ("*** ERROR : variable "++ symbol ++" is not bound.")

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
evalAdd :: Ast -> Ast -> Env -> Either String (Ast, Env)
evalAdd a b env = do
  (aValue, env1) <- evalAst a env
  (bValue, env2) <- evalAst b env1
  case (aValue, bValue) of
    (Value (SInt x), Value (SInt y)) -> Right (Value (SInt (x + y)), env2)
    (Value (SSym symA), Value (SInt y)) -> do
      symbolValueA <- lookupSymbol symA env2
      case symbolValueA of
        Value (SInt x) -> Right (Value (SInt (x + y)), env2)
        _              -> Left "Error addition: symbol value is not an integer"
    _ -> Left "Error addition: invalid value types"

-- soustraction
evalSub :: Ast -> Ast -> Env -> Either String (Ast, Env)
evalSub a b env = do
  (aValue, env1) <- evalAst a env
  (bValue, env2) <- evalAst b env1
  case (aValue, bValue) of
    (Value (SInt x), Value (SInt y)) -> Right (Value (SInt (x - y)), env2)
    (Value (SSym symA), Value (SInt y)) -> do
      symbolValueA <- lookupSymbol symA env2
      case symbolValueA of
        Value (SInt x) -> Right (Value (SInt (x - y)), env2)
        _ -> Left "Error subtraction: symbol value is not an integer"
    _ -> Left "Error subtraction: invalid value types"

-- multiplication
evalMul :: Ast -> Ast -> Env -> Either String (Ast, Env)
evalMul a b env = do
  (aValue, env1) <- evalAst a env
  (bValue, env2) <- evalAst b env1
  case (aValue, bValue) of
    (Value (SInt x), Value (SInt y)) -> Right (Value (SInt (x * y)), env2)
    (Value (SSym symA), Value (SInt y)) -> do
      symbolValueA <- lookupSymbol symA env2
      case symbolValueA of
        Value (SInt x) -> Right (Value (SInt (x * y)), env2)
        _ -> Left "Error multiplication: symbol value is not an integer"
    _ -> Left "Error multiplication: invalid value types"

-- division
evalDiv :: Ast -> Ast -> Env -> Either String (Ast, Env)
evalDiv a b env = do
  (aValue, env1) <- evalAst a env
  (bValue, env2) <- evalAst b env1
  case (aValue, bValue) of
    (Value (SInt _), Value (SInt 0)) -> Left "Error division by zero"
    (Value (SInt x), Value (SInt y)) -> Right (Value (SInt (x `div` y)), env2)
    (Value (SSym symA), Value (SInt y)) -> do
      symbolValueA <- lookupSymbol symA env2
      case symbolValueA of
        Value (SInt x) -> Right (Value (SInt (x `div` y)), env2)
        _              -> Left "Error division: symbol value is not an integer"
    _ -> Left "Error division: invalid value types"

-- modulo
evalMod :: Ast -> Ast -> Env -> Either String (Ast, Env)
evalMod a b env = do
  (aValue, env1) <- evalAst a env
  (bValue, env2) <- evalAst b env1
  case (aValue, bValue) of
    (Value (SInt _), Value (SInt 0)) -> Left "Error modulo by zero"
    (Value (SInt x), Value (SInt y)) -> Right (Value (SInt (x `mod` y)), env2)
    (Value (SSym symA), Value (SInt y)) -> do
      symbolValueA <- lookupSymbol symA env2
      case symbolValueA of
        Value (SInt x) -> Right (Value (SInt (x `mod` y)), env2)
        _              -> Left "Error modulo: symbol value is not an integer"
    _ -> Left "Error modulo: invalid value types"

evalAst :: Ast -> Env -> Either String (Ast, Env)
evalAst (Value v) env = Right (Value v, env)
evalAst (Symbol var) env =
  case tryReadVar var env of
    Left (v, newEnv) -> Right (v, newEnv)
    Right str -> Left str
evalAst (Define var expr) env = do
  (exprValue, newEnv) <- evalAst expr env
  Right (Define var exprValue, newEnv)
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
evalAst (Call "nested" [Define var expr, expr2]) env = do
  (exprValue, newEnv) <- evalAst expr env
  let updatedEnv = Map.insert var exprValue env
  evalAst expr2 updatedEnv
evalAst _ _ = Left "error"

evalBinOp ::
     (Ast -> Ast -> Env -> Either String (Ast, Env))
  -> Ast
  -> Ast
  -> Env
  -> Either String (Ast, Env)
evalBinOp op a b env = do
  (aValue, env1) <- evalAst a env
  (bValue, env2) <- evalAst b env1
  (result, env3) <- op aValue bValue env2
  Right (result, env3)


evalComparison ::
     (Int -> Int -> Bool) -> Ast -> Ast -> Env -> Either String (Ast, Env)
evalComparison compOp a b env = do
  (aValue, env1) <- evalAst a env
  (bValue, env2) <- evalAst b env1
  case (aValue, bValue) of
    (Value (SInt x), Value (SInt y)) -> Right (Value (SBool (compOp x y)), env2)
    (Value (SSym symA), Value (SInt y)) -> do
      symbolValueA <- lookupSymbol symA env2
      case symbolValueA of
        Value (SInt x) -> Right (Value (SBool (compOp x y)), env2)
        _ -> Left "Error comparison: symbol value is not an integer"
    _ -> Left "Error comparison: invalid value types"


evalIf :: Ast -> Ast -> Ast -> Env -> Either String (Ast, Env)
evalIf condExpr trueExpr falseExpr env = do
  (condValue, env1) <- evalAst condExpr env
  case condValue of
    Value (SBool condition) ->
      if condition
        then evalAst trueExpr env1
        else evalAst falseExpr env1
    _ -> Left "error if"
