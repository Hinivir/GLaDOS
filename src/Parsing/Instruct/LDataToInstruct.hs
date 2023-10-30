{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parsing/InstructTree/LDataToInstruct
-}

module Parsing.Instruct.LDataToInstruct
  (
    convertLDataToInstruct
  ) where

import ParserStatus
  (
    createParserStatusOk,
    createParserStatusError,
    ParserStatus
  )

import Parsing.Instruct
  (
    Value(..),
    Operation(..),
    Instruction(..),
    Instructions,
    Env
  )

import Parsing.LDataTree
  (
    LData(..),
  )

ldataToValue :: [LData] -> Value
ldataToValue (LDataInt x (_, _) : _) = Number x
ldataToValue (LDataFloat x (_, _): _) = Float x
ldataToValue (LDataBool x (_, _): _) = Boolean x
ldataToValue (LDataString x (_, _): _) = String x
ldataToValue _ = ValueUndefined

handleVar :: String -> [LData] -> Env -> (Maybe Instructions, Env, ParserStatus)
handleVar x y env = (Just [], (x,ldataToValue y) : env, createParserStatusOk)

convertLDataToInstruct :: [LData] -> Env -> (Maybe Instructions, Env, ParserStatus)
convertLDataToInstruct (LDataGroup x (_, _): _) env = convertLDataToInstruct x env
convertLDataToInstruct (LDataInt x (_, _) : _) env =
  (Just [Push (Number x)], env, createParserStatusOk)
convertLDataToInstruct (LDataFloat x (_, _) : _) env =
  (Just [Push (Float x)], env, createParserStatusOk)
convertLDataToInstruct (LDataBool x (_, _) : _) env =
  (Just [Push (Boolean x)], env, createParserStatusOk)
convertLDataToInstruct (LDataSymbol "add" (_, _) : _) env =
  (Just [Push (Op Add)], env, createParserStatusOk)
convertLDataToInstruct (LDataSymbol "sub" (_, _) : _) env =
  (Just [Push (Op Sub)], env, createParserStatusOk)
convertLDataToInstruct (LDataSymbol "mul" (_, _) : _) env =
  (Just [Push (Op Mul)], env, createParserStatusOk)
convertLDataToInstruct (LDataSymbol "div" (_, _) : _) env =
  (Just [Push (Op Div)], env, createParserStatusOk)
convertLDataToInstruct (LDataSymbol "eq" (_, _) : _) env =
  (Just [Push (Op Eq)], env, createParserStatusOk)
convertLDataToInstruct (LDataSymbol "less" (_, _) : _) env =
  (Just[Push (Op Less)], env, createParserStatusOk)
convertLDataToInstruct (LDataList x (_, _) : _) env = convertLDataToInstruct x env
convertLDataToInstruct (LDataDict x (_, _) : _) env = convertLDataToInstruct x env
convertLDataToInstruct (LDataTuple x (_, _) : _) env = convertLDataToInstruct x env
convertLDataToInstruct (LDataSymbol "Lipdo" _ : LDataSymbol x _ : _) env =
  (Nothing, env, createParserStatusError "Error" ("Symbol Not Know " ++ x) 0 0)
convertLDataToInstruct (LDataSymbol "Lipbe" _ : LDataSymbol x _ : LDataSymbol ":" _ : LDataGroup y _ : _) env =
  handleVar x y env
convertLDataToInstruct (LDataSymbol x (l, c): _) _ =
  (Nothing, [], createParserStatusError "Error" ("Symbol Not Know " ++ x) l c)
convertLDataToInstruct (LDataString x (_, _) : _) _ =
  (Just [Push (String x)], [], createParserStatusOk)
convertLDataToInstruct [] _ =
  (Nothing, [], createParserStatusError "Error" "Can't Convert" 0 0)
convertLDataToInstruct (LDataUndefined : _) _ =
  (Nothing, [], createParserStatusError "Error" "Can't Convert" 0 0)

{-- Todo
[LDataGroup [LDataSymbol "Lipdo" (1,1),
LDataSymbol "add" (7,1),
LDataSymbol "a" (11,1),
LDataSymbol "b" (13,1),
LDataSymbol ":" (14,1)] (0,0),
LDataGroup [LDataSymbol "a" (5,2),
LDataSymbol "+" (7,2),
LDataSymbol "b" (9,2)] (0,0),
LDataGroup [LDataSymbol "Lipdo" (1,3),
LDataSymbol "main" (7,3),
LDataSymbol ":" (11,3)] (0,0),
LDataGroup [LDataSymbol "add" (5,4),
LDataInt 3 (9,4),
LDataInt 4 (11,4)] (0,0)]
--}
