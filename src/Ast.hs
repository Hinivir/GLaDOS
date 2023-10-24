{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Ast
-}

{-|
Module      : Ast
Description : Abstract Syntax Tree (AST) Definition and Evaluation

This module defines the abstract syntax tree (AST) data structure and provides functions for evaluating and manipulating it. The AST represents expressions, variables, and operations in a simple programming language.

== Data Types

- 'Ast': The primary data type representing nodes in the AST. It includes constructors for defining variables, calling functions, storing values, and working with symbols.

- 'Env': An alias for a map that associates variable names with AST values.

== Functions

- 'sexprToAst': Converts an S-expression (a simple language representation) into an AST. It handles various S-expression forms and constructs the corresponding AST nodes.

- 'evalAdd': Evaluates addition operations in the AST. It supports numeric values and symbolic variables.

- 'evalSub': Evaluates subtraction operations in the AST. It supports numeric values and symbolic variables.

- 'evalMul': Evaluates multiplication operations in the AST. It supports numeric values and symbolic variables.

- 'evalDiv': Evaluates division operations in the AST. It supports numeric values and symbolic variables. It also handles division by zero errors.

- 'evalMod': Evaluates modulo operations in the AST. It supports numeric values and symbolic variables. It also handles modulo by zero errors.

- 'evalAst': Evaluates an entire AST. It handles various operations and expressions, variable lookups, and conditionals. It provides error handling for invalid operations and expressions.

== Usage Example

To use this module, you can define an AST and evaluate it as follows:

1. Define an AST using 'Ast' constructors.

2. Create an environment ('Env') that associates variable names with their values.

3. Call 'evalAst' with your AST and the environment to evaluate it.

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

-- | The AST data type.
data Ast
  = Define String Ast
  -- | Define a variable
  | DefineFunction String [String] Ast
  -- | Define a function variable
  | Call String [Ast]
  -- | Call a function
  | Value SExpr
  -- | A value
  | Symbol String
  -- | A symbol
 deriving (Show, Eq)

-- | The environment data type that is Map containg in key a String and
-- a Ast for the value.
type Var = (String, Ast)
type Func = (String, [String], Ast)
type Env = Map.Map String (Either Var Func)

-- | tryReadVar searches for a variable in the environment (Env). *
-- If found, it returns a pair with the value and the original environment.
-- If not found, it provides an error message for an unknown symbol, crucial for variable lookups in the environment.

-- Redefine the tryReadVar function to handle the new Env type
tryReadVar :: String -> Env -> Either (Ast, Env) String
tryReadVar key m =
  case Map.lookup key m of
    Nothing -> Right $ "Unknown symbol " ++ key
    Just (Left (_, varAst))  -> Left (varAst, m)
    Just (Right _) -> Right $ "*** ERROR: Variable " ++ key ++ " is not bound."

-- Redefine the lookupSymbol function to handle the new Env type
lookupSymbol :: String -> Env -> Either String Ast
lookupSymbol symbol env =
  case Map.lookup symbol env of
    Just (Left (_, value)) -> Right value
    Just (Right _) -> Left ("*** ERROR: Variable " ++ symbol ++ " not bound")
    Nothing        -> Left ("*** ERROR: Variable " ++ symbol ++ " not bound")

-- Function to convert a list of SExpr into a list of strings
sexprToAstFunctionArguments :: [SExpr] -> Either String [String]
sexprToAstFunctionArguments [] = Right []
sexprToAstFunctionArguments (SSym arg : t) =
  case sexprToAstFunctionArguments t of
    Left err -> Left err
    Right rest -> Right (arg : rest)
sexprToAstFunctionArguments _ = Left "*** ERROR : (sexprToAstFunctionArguments)."

-- Function to convert a SExpr to an Ast
sexprToAst :: SExpr -> Either String Ast
sexprToAst (SList [SSym "define", SSym var, expr]) =
  sexprToAst expr >>= \exprAst -> Right (Define var exprAst)
sexprToAst (SList [SSym "define", SList (SSym var : args), expr]) =
  sexprToAst expr >>= \exprAst ->
    case sexprToAstFunctionArguments args of
      Left err -> Left err
      Right args2 -> Right (DefineFunction var args2 exprAst)
sexprToAst (SList [x]) = sexprToAst x
sexprToAst (SList (SSym func : args)) =
  case mapM sexprToAst args of
    Left str -> Left str
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


-- Function to evaluate binary operations
evalBinaryOp ::
  (Int -> Int -> Int) -> Ast -> Ast -> Env -> Either String (Ast, Env)
evalBinaryOp op a b env = do
  let evalResult = do
        (aValue, env1) <- evalAst a env
        (bValue, env2) <- evalAst b env1
        case (aValue, bValue) of
          (Value (SInt x), Value (SInt y)) -> Right (Value (SInt (x `op` y)), env2)
          (Value (SSym symA), Value (SInt y)) -> do
            symbolValueA <- lookupSymbol symA env2
            case symbolValueA of
              Value (SInt x) -> Right (Value (SInt (x `op` y)), env2)
              _ -> Left "Error: symbol value is not an integer"
          _ -> Left "Error: invalid value types"
  evalResult

-- Functions to handle specific binary operations
evalAdd :: Ast -> Ast -> Env -> Either String (Ast, Env)
evalAdd = evalBinaryOp (+)

evalSub :: Ast -> Ast -> Env -> Either String (Ast, Env)
evalSub = evalBinaryOp (-)

evalMul :: Ast -> Ast -> Env -> Either String (Ast, Env)
evalMul = evalBinaryOp (*)

evalDiv :: Ast -> Ast -> Env -> Either String (Ast, Env)
evalDiv (Value (SInt _)) (Value (SInt 0)) _ = Left "Error division by zero"
evalDiv a b env = evalBinaryOp div a b env

evalMod :: Ast -> Ast -> Env -> Either String (Ast, Env)
evalMod (Value (SInt _)) (Value (SInt 0)) _ = Left "Error modulo by zero"
evalMod a b env = evalBinaryOp mod a b env

-- Function to evaluate an Ast
evalAst :: Ast -> Env -> Either String (Ast, Env)
evalAst (Value v) env = Right (Value v, env)
evalAst (Symbol var) env =
  case tryReadVar var env of
    Left (v, newEnv) -> Right (v, newEnv)
    Right str -> Left str
evalAst (Define var expr) env = do
  (exprValue, newEnv) <- evalAst expr env
  Right (Define var exprValue, Map.insert var (Left (var, exprValue)) newEnv)
evalAst (Call "+" [a, b]) env = evalAdd a b env
evalAst (Call "-" [a, b]) env = evalSub a b env
evalAst (Call "*" [a, b]) env = evalMul a b env
evalAst (Call "div" [a, b]) env = evalDiv a b env
evalAst (Call "mod" [a, b]) env = evalMod a b env
evalAst (Call ">" [a, b]) env = evalComparison (>) a b env
evalAst (Call "<" [a, b]) env = evalComparison (<) a b env
evalAst (Call "eq?" [a, b]) env = evalComparison (==) a b env
evalAst (Call "if" [condExpr, trueExpr, falseExpr]) env =
  evalIf condExpr trueExpr falseExpr env
evalAst (Call "nested" [Define var expr, expr2]) env = do
  (exprValue, _) <- evalAst expr env
  let updatedEnv = Map.insert var (Left (var, exprValue)) env
  evalAst expr2 updatedEnv
evalAst (Call "nested" [DefineFunction name args ins, expr2]) env = do
  let updatedEnv = Map.insert name (Right (name, args, ins)) env
  evalAst expr2 updatedEnv
evalAst (Call name args) env = evalFunction (Call name args) env
evalAst _ _ = Left "error while evaluating ast"

evalFunction :: Ast -> Env -> Either String (Ast, Env)
evalFunction (Call name args) env =
  case Map.lookup name env of
    Just (Right (_, funcArgs, funcIns)) ->
      if length args == length funcArgs
        then do
          let updatedEnv = Map.insert name (Right (name, funcArgs, funcIns)) env
          let updatedEnv2 = foldl (\acc (arg, value) -> Map.insert arg (Left (arg, value)) acc) updatedEnv (zip funcArgs args)
          evalAst funcIns updatedEnv2
        else Left "Error: wrong number of arguments"
    _ -> Left "Error: function not found"

-- Function to compare values
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

-- Function to evaluate comparison operations
evalComparison ::
  (Int -> Int -> Bool) -> Ast -> Ast -> Env -> Either String (Ast, Env)
evalComparison op a b env = do
  (aValue, env1) <- evalAst a env
  (bValue, env2) <- evalAst b env1
  compareValues op aValue bValue env2

-- Function to handle the "if" statement
evalIf :: Ast -> Ast -> Ast -> Env -> Either String (Ast, Env)
evalIf condExpr trueExpr falseExpr env = do
  (condValue, env1) <- evalAst condExpr env
  case condValue of
    Value (SBool condition) ->
      if condition
        then evalAst trueExpr env1
        else evalAst falseExpr env1
    _ -> Left "error if"
