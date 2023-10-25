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
  ) where

import SExpr (SExpr (..))

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
type Func = ([String], Ast)

data EnvVar =
  Var Ast
  | Function Func
  deriving (Show, Eq)

type Env = [(String, EnvVar)]

-- | tryReadVar searches for a variable in the environment (Env). *
-- If found, it returns a pair with the value and the original environment.
-- If not found, it provides an error message for an unknown symbol, crucial for variable lookups in the environment.

-- Function to convert a list of SExpr into a list of strings
sexprToAstFunctionArguments :: [SExpr] -> Either String [String]
sexprToAstFunctionArguments [] = Right []
sexprToAstFunctionArguments (SSym arg : t) =
  case sexprToAstFunctionArguments t of
    Left err -> Left err
    Right rest -> Right (arg : rest)
sexprToAstFunctionArguments _ = Left "***ERROR: (sexprToAstFunctionArguments)."

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
