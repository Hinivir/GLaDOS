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
  | Call String [Ast]
  -- | Call a function
  | Value SExpr
  -- | A value
  | Symbol String
  -- | A symbol
 deriving (Show, Eq)

-- | The environment data type that is Map containg in key a String and
-- a Ast for the value.
type Env = Map.Map String Ast

-- | Try to read a variable from the environment.
-- | Return a string if there is an Unknown symbol.
-- | Return a list of Ast and the environment if the variable is found.
tryReadVar :: String -> Env -> Either (Ast, Env) String
tryReadVar key m =
  case Map.lookup key m of
    Nothing -> Right $ "Unknown symbol " ++ key
    Just v  -> Left (v, m)

-- | Lookup a symbol in the environment.
-- | Return a string if there is an Unknown symbol.
-- | Return a Ast if the variable is found.
lookupSymbol :: String -> Env -> Either String Ast
lookupSymbol symbol env =
  case Map.lookup symbol env of
    Just value -> Right value
    Nothing    -> Left ("*** ERROR : variable "++ symbol ++" is not bound.")

-- | Convert a SExpr to a Ast.
-- | Return a string if there is an error.
-- | Return a Ast if the conversion is successful.
sexprToAst :: SExpr -> Either String Ast
sexprToAst (SList [SSym "define", SSym var, SList[SSym a, SSym b]]) = do
  Right (Define var (Call a [Symbol b]))
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

-- | Evaluate a binary operation.
-- | Return a string if there is an error.
-- | Return a Ast and the environment if the evaluation is successful.
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

-- | Handle the evaluation of an addition.
-- | Return a string if there is an error.
-- | Return a Ast and the environment if the evaluation is successful
evalAdd :: Ast -> Ast -> Env -> Either String (Ast, Env)
evalAdd = evalBinaryOp (+)

-- | Handle the evaluation of a subtraction.
-- | Return a string if there is an error.
-- | Return a Ast and the environment if the evaluation is successful
evalSub :: Ast -> Ast -> Env -> Either String (Ast, Env)
evalSub = evalBinaryOp (-)

-- | Handle the evaluation of a multiplication.
-- | Return a string if there is an error.
-- | Return a Ast and the environment if the evaluation is successful
evalMul :: Ast -> Ast -> Env -> Either String (Ast, Env)
evalMul = evalBinaryOp (*)

-- | Handle the evaluation of a division.
-- | Return a string if there is an error.
-- | Return a Ast and the environment if the evaluation is successful
evalDiv :: Ast -> Ast -> Env -> Either String (Ast, Env)
evalDiv (Value (SInt _)) (Value (SInt 0)) _ = Left "Error division by zero"
evalDiv a b env = evalBinaryOp div a b env

-- | Handle the evaluation of a modulo.
-- | Return a string if there is an error.
-- | Return a Ast and the environment if the evaluation is successful
evalMod :: Ast -> Ast -> Env -> Either String (Ast, Env)
evalMod (Value (SInt _)) (Value (SInt 0)) _ = Left "Error modulo by zero"
evalMod a b env = evalBinaryOp mod a b env

-- | Evaluate a Ast.
-- | Return a string if there is an error.
-- | Return a Ast and the environment if the evaluation is successful
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

-- | Evaluate a binary operation.
-- | Return a string if there is an error.
-- | Return a Ast and the environment if the evaluation is successful.
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

-- | Compare two values.
-- | Return a string if there is an error.
-- | Return a Ast and the environment if the evaluation is successful.
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

-- | Evaluate a comparison.
-- | Return a string if there is an error.
-- | Return a Ast and the environment if the evaluation is successful.
evalComparison ::
     (Int -> Int -> Bool) -> Ast -> Ast -> Env -> Either String (Ast, Env)
evalComparison op a b env = do
  (aValue, env1) <- evalAst a env
  (bValue, env2) <- evalAst b env1
  compareValues op aValue bValue env2

-- | Evaluate a if.
-- | Return a string if there is an error.
-- | Return a Ast and the environment if the evaluation is successful.
evalIf :: Ast -> Ast -> Ast -> Env -> Either String (Ast, Env)
evalIf condExpr trueExpr falseExpr env = do
  (condValue, env1) <- evalAst condExpr env
  case condValue of
    Value (SBool condition) ->
      if condition
        then evalAst trueExpr env1
        else evalAst falseExpr env1
    _ -> Left "error if"
