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
  | Function String
 deriving (Show, Eq)

type Env = Map.Map String Ast

-- tryReadVar searches for a variable in the environment (Env). *
-- If found, it returns a pair with the value and the original environment.
-- If not found, it provides an error message for an unknown symbol, crucial for variable lookups in the environment.

tryReadVar :: String -> Env -> Either (Ast, Env) String
tryReadVar key m =
  case Map.lookup key m of
    Nothing -> Right $ "Unknown symbol " ++ key
    Just v  -> Left (v, m)

-- lookupSymbol looks up a symbol in the environment (Env) and returns its associated Ast value.
-- It's a key part of symbol resolution and allows the code to access values associated with symbols in the environment.

lookupSymbol :: String -> Env -> Either String Ast
lookupSymbol symbol env =
  case Map.lookup symbol env of
    Just value -> Right value
    Nothing    -> Left ("*** ERROR : variable "++ symbol ++" is not bound.")

sexprToAst :: SExpr -> Either String Ast
sexprToAst (SList [SSym "define", SSym var, expr]) =
  sexprToAst expr >>= \exprAst -> Right (Define var exprAst)
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

-- evalBinaryOp is a utility that supports binary operation evaluation (e.g., addition, subtraction) between two "Ast" values,
-- accommodating various value types like integers and symbols, guaranteeing consistent binary operation behavior.

evalBinaryOp ::
     (Int -> Int -> Int) -> Ast -> Ast -> Env -> Either String (Ast, Env)
evalBinaryOp op a b env =
  let Right (aValue, env1) = evalAst a env
      Right (bValue, env2) = evalAst b env1
  in case (aValue, bValue) of
    (Value (SInt x), Value (SInt y)) -> Right (Value (SInt (x `op` y)), env2)
    (Value (SSym symA), Value (SInt y)) ->
      case lookupSymbol symA env2 of
        Right (Value (SInt x)) -> Right (Value (SInt (x `op` y)), env2)
        _              -> Left "Error: symbol value is not an integer"
    _ -> Left "Error: invalid value types"

-- Addition
evalAdd :: Ast -> Ast -> Env -> Either String (Ast, Env)
evalAdd = evalBinaryOp (+)

-- Subtraction
evalSub :: Ast -> Ast -> Env -> Either String (Ast, Env)
evalSub = evalBinaryOp (-)

-- Multiplication
evalMul :: Ast -> Ast -> Env -> Either String (Ast, Env)
evalMul = evalBinaryOp (*)

-- Division
evalDiv :: Ast -> Ast -> Env -> Either String (Ast, Env)
evalDiv (Value (SInt _)) (Value (SInt 0)) _ = Left "Error division by zero"
evalDiv a b env = evalBinaryOp div a b env

-- Modulo
evalMod :: Ast -> Ast -> Env -> Either String (Ast, Env)
evalMod (Value (SInt _)) (Value (SInt 0)) _ = Left "Error modulo by zero"
evalMod a b env = evalBinaryOp mod a b env

-- evalAst handles nested scopes, allowing variable definition with "Define" and usage in "expr2," facilitating nested scopes and variable shadowing.
-- It's valuable for creating lexical scoping in the language.

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
  (exprValue, _) <- evalAst expr env
  let updatedEnv = Map.insert var exprValue env
  evalAst expr2 updatedEnv
evalAst _ _ = Left "error"

-- This is a versatile binary operation evaluator for addition, subtraction, multiplication, division, and modulo.
-- It employs operation-specific evaluation functions (e.g., evalAdd, evalSub) to process operands, simplifying the handling of binary operations

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

-- compareValues compares two "Ast" values using a custom comparison function for integers.
-- It's a modular way to handle comparisons like >, <, and eq?.

compareValues ::
     (Int -> Int -> Bool) -> Ast -> Ast -> Env -> Either String (Ast, Env)
compareValues op aValue bValue env =
  case (aValue, bValue) of
    (Value (SInt x), Value (SInt y)) -> Right (Value (SBool (op x y)), env)
    (Value (SSym symA), Value (SInt y)) -> do
      symbolValueA <- lookupSymbol symA env
      case symbolValueA of
        Value (SInt x) -> Right (Value (SBool (op x y)), env)
        _ -> Left "Error comparison: symbol value is not an integer"
    _ -> Left "Error comparison: invalid value types"

-- evalComparison provides a consistent way to evaluate comparison operations (e.g., >, <, eq?) on "Ast" values using the "compareValues" function.
-- It abstracts the comparison logic, promoting code consistency for various "Ast" types.

evalComparison ::
     (Int -> Int -> Bool) -> Ast -> Ast -> Env -> Either String (Ast, Env)
evalComparison op a b env = do
  (aValue, env1) <- evalAst a env
  (bValue, env2) <- evalAst b env1
  compareValues op aValue bValue env2

-- evalIf handles conditional expressions using if. It evaluates the condition and, depending on whether it's true or false, evaluates either the true or false branches.
-- It's essential for controlling program flow and making decisions in the code.

evalIf :: Ast -> Ast -> Ast -> Env -> Either String (Ast, Env)
evalIf condExpr trueExpr falseExpr env = do
  (condValue, env1) <- evalAst condExpr env
  case condValue of
    Value (SBool condition) ->
      if condition
        then evalAst trueExpr env1
        else evalAst falseExpr env1
    _ -> Left "error if"
