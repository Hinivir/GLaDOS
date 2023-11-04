{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Parsing/InstructTree/LDataToInstruct
-}

module Parsing.Instruct.LDataToInstruct
  (
    convertLDataToInstruct,
    handleVar,
    handleLDataGroup,
    handleOpInt,
    handleGroupOpInt,
    handleIntOpGroup,
    handleOpFloat,
    handleGroupOpFloat,
    handleFloatOpGroup,
    getOp,
    getOp2
  ) where

import ParserStatus
  (
    createParserStatusOk,
    createParserStatusError,
    ParserStatus
  )

import Vm
  (
    Operation(..),
    Instructions,
    Value(..),
    Env,
    Instruction(..),
  )

import Parsing.LDataTree
  (
    LData(..),
  )

handleVar :: String -> [LData] -> Env -> Instructions ->
  (Maybe Instructions, Env, ParserStatus)
handleVar x (LDataGroup [LDataInt y _] _ : z) env inst =
  convertLDataToInstruct z ((x, Number y) : env) inst
handleVar x (LDataGroup [LDataFloat y _] _ : z) env inst =
  convertLDataToInstruct z ((x, Float y) : env) inst
handleVar x (LDataGroup [LDataBool y _] _ : z) env inst =
  convertLDataToInstruct z ((x, Boolean y) : env) inst
handleVar x (LDataGroup [LDataString y _] _ : z) env inst =
  convertLDataToInstruct z ((x, String y) : env) inst
handleVar x _ _ _ = (Nothing, [], createParserStatusError "Error"
  ("handleVar " ++ show x) 0 0)

getOp :: String -> Operation
getOp x = case x of
  "+" -> Add
  "-" -> Sub
  "*" -> Mul
  "div" -> Div
  "mod" -> Mod
  _ -> getOp2 x

getOp2 :: String -> Operation
getOp2 x = case x of
  "==" -> Eq
  "<" -> Less
  ">" -> Greater
  "<=" -> LessEq
  ">=" -> GreaterEq
  _ -> error "Invalid operation"

handleLDataGroup :: [LData] -> [LData] -> Env -> Instructions ->
  (Maybe Instructions, Env, ParserStatus)
handleLDataGroup (LDataSymbol "Lipbe" _ : LDataSymbol x _ : LDataSymbol ":" _ :
  _) group env inst = handleVar x group env inst
handleLDataGroup (LDataSymbol "Lipdo" _ : LDataSymbol "main" _ : _) y env inst =
  convertLDataToInstruct y env inst
handleLDataGroup (LDataSymbol x _ : LDataSymbol op _ : LDataInt y _ : rest)
  z env inst = convertLDataToInstruct (rest ++ z) env (inst ++ [PushEnv x,
    Push (Number y), Push (Op (getOp op)), Call])
handleLDataGroup (LDataSymbol x _ : LDataSymbol op _ : LDataFloat y _ : rest)
  z env inst = convertLDataToInstruct (rest ++ z) env (inst ++ [PushEnv x,
    Push (Float y), Push (Op (getOp op)), Call])
handleLDataGroup (LDataInt x _ : LDataSymbol op _ : LDataSymbol y _ : rest)
  z env inst = convertLDataToInstruct (rest ++ z) env (inst ++ [Push (Number x),
  PushEnv y, Push (Op (getOp op)), Call])
handleLDataGroup (LDataFloat x _ : LDataSymbol op _ : LDataSymbol y _ : rest)
  z env inst = convertLDataToInstruct (rest ++ z) env (inst ++ [Push (Float x),
  PushEnv y, Push (Op (getOp op)), Call])
handleLDataGroup (LDataGroup x _ : rest) y env inst =
  handleLDataGroup x (rest ++ y) env inst
handleLDataGroup (LDataInt x _ : LDataSymbol op _ : LDataInt y' _ : rest)
  y env inst = handleOpInt x op y' (rest ++ y) env inst
handleLDataGroup (LDataSymbol x _ : rest) y env inst =
  convertLDataToInstruct (rest ++ y) env (inst ++ [PushEnv x])
handleLDataGroup (LDataInt x _ : LDataSymbol op _ : LDataGroup y _ : rest)
  z env inst = handleIntOpGroup x op y (rest ++ z) env inst
handleLDataGroup (LDataFloat x _ : LDataSymbol op _ : LDataFloat y' _ : rest)
  y env inst = handleOpFloat x op y' (rest ++ y) env inst
handleLDataGroup (LDataFloat x _ : LDataSymbol op _ : LDataGroup y _ : rest)
  z env inst = handleFloatOpGroup x op y (rest ++ z) env inst
handleLDataGroup [] [] env inst = convertLDataToInstruct [] env inst
handleLDataGroup [] x env inst = convertLDataToInstruct x env inst
handleLDataGroup x _ _ _ =
  (Nothing, [], createParserStatusError "Error" ("handleLDataGroup " ++ show x)
    0 0)

handleOpInt :: Int -> String -> Int -> [LData] -> Env -> Instructions ->
  (Maybe Instructions, Env, ParserStatus)
handleOpInt x op y rest env inst =
  convertLDataToInstruct rest env (inst ++ [Push (Number x), Push (Number y),
    Push (Op (getOp op)), Call])

handleGroupOpInt :: [LData] -> String -> Int -> [LData] -> Env -> Instructions ->
  (Maybe Instructions, Env, ParserStatus)
handleGroupOpInt x op y rest env inst =
  case handleLDataGroup x rest env inst of
    (Nothing, _, _) -> (Nothing, [], createParserStatusError "error"
      ("error handle " ++ op) 0 0)
    (Just inst', env', _) -> (Just (init inst' ++ [Push (Number y),
      Push (Op (getOp op)), Call]), env', createParserStatusOk)

handleIntOpGroup :: Int -> String -> [LData] -> [LData] -> Env -> Instructions ->
  (Maybe Instructions, Env, ParserStatus)
handleIntOpGroup x op y rest env inst =
  case handleLDataGroup y rest env inst of
    (Nothing, _, _) -> (Nothing, [], createParserStatusError "error"
      ("error handle " ++ op) 0 0)
    (Just inst', env', _) -> (Just (init inst' ++ [Push (Number x),
      Push (Op (getOp op)), Call]), env', createParserStatusOk)

handleOpFloat :: Float -> String -> Float -> [LData] -> Env -> Instructions ->
  (Maybe Instructions, Env, ParserStatus)
handleOpFloat x op y rest env inst =
  convertLDataToInstruct rest env (inst ++ [Push (Float x), Push (Float y),
    Push (Op (getOp op)), Call])

handleGroupOpFloat :: [LData] -> String -> Float -> [LData] -> Env ->
  Instructions -> (Maybe Instructions, Env, ParserStatus)
handleGroupOpFloat x op y rest env inst =
  case handleLDataGroup x rest env inst of
    (Nothing, _, _) -> (Nothing, [], createParserStatusError "error"
      ("error handle " ++ op) 0 0)
    (Just inst', env', _) -> (Just (init inst' ++ [Push (Float y),
      Push (Op (getOp op)), Call]), env', createParserStatusOk)

handleFloatOpGroup :: Float -> String -> [LData] -> [LData] -> Env ->
  Instructions -> (Maybe Instructions, Env, ParserStatus)
handleFloatOpGroup x op y rest env inst =
  case handleLDataGroup y rest env inst of
    (Nothing, _, _) -> (Nothing, [], createParserStatusError "error"
      ("error handle " ++ op) 0 0)
    (Just inst', env', _) -> (Just (init inst' ++ [Push (Float x),
      Push (Op (getOp op)), Call]), env', createParserStatusOk)

convertLDataToInstruct :: [LData] -> Env -> Instructions ->
  (Maybe Instructions, Env, ParserStatus)
convertLDataToInstruct (LDataGroup x _ : LDataSymbol op _ : LDataInt y' _ :
  rest) env inst = handleGroupOpInt x op y' rest env inst
convertLDataToInstruct (LDataGroup x _ : LDataSymbol op _ : LDataFloat y' _ :
  rest) env inst = handleGroupOpFloat x op y' rest env inst
convertLDataToInstruct (LDataGroup x _ : y) env inst =
  handleLDataGroup x y env inst
convertLDataToInstruct (LDataInt x _ : LDataSymbol "+" _ : LDataInt y _ : rest)
  env inst =
  convertLDataToInstruct rest env (inst ++ [Push (Number x), Push (Number y),
    Push (Op Add), Call])
convertLDataToInstruct (LDataInt x _ : LDataSymbol op _ : LDataInt y _ : rest)
  env inst = handleOpInt x op y rest env inst
convertLDataToInstruct (LDataFloat x _ : LDataSymbol op _ : LDataFloat y _ :
  rest) env inst = handleOpFloat x op y rest env inst
convertLDataToInstruct (LDataSymbol "+" _ : LDataGroup x _ : rest) env inst =
  case handleLDataGroup x rest env inst of
    (Just inst', env', _) -> (Just (init inst' ++ [Push (Op Add), Call, Ret]),
      env', createParserStatusOk)
    _ -> (Nothing, [], createParserStatusError "error" "error handle +" 0 0)
convertLDataToInstruct (LDataSymbol "-" _ : LDataGroup x _ : rest) env inst =
  case handleLDataGroup x rest env inst of
    (Just inst', env', _) -> (Just (init inst' ++ [Push (Op Sub), Call, Ret]),
      env', createParserStatusOk)
    _ -> (Nothing, [], createParserStatusError "error" "error handle -" 0 0)
convertLDataToInstruct (LDataSymbol "*" _ : LDataGroup x _ : rest) env inst =
  case handleLDataGroup x rest env inst of
    (Just inst', env', _) -> (Just (init inst' ++ [Push (Op Mul), Call, Ret]),
      env', createParserStatusOk)
    _ -> (Nothing, [], createParserStatusError "error" "error handle *" 0 0)
convertLDataToInstruct (LDataSymbol "div" _ : LDataGroup x _ : rest) env inst =
  case handleLDataGroup x rest env inst of
    (Just inst', env', _) -> (Just (init inst' ++ [Push (Op Div), Call, Ret]),
      env', createParserStatusOk)
    _ -> (Nothing, [], createParserStatusError "error" "error handle div" 0 0)
convertLDataToInstruct (LDataSymbol "mod" _ : LDataGroup x _ : rest) env inst =
  case handleLDataGroup x rest env inst of
    (Just inst', env', _) -> (Just (init inst' ++ [Push (Op Mod), Call, Ret]),
      env', createParserStatusOk)
    _ -> (Nothing, [], createParserStatusError "error" "error handle mod" 0 0)
convertLDataToInstruct (LDataSymbol "==" (l, c) : LDataGroup x _ : rest)
  env inst = case handleLDataGroup x rest env inst of
    (Just inst', env', _) -> (Just (init inst' ++ [Push (Op Eq), Call, Ret]),
      env', createParserStatusOk)
    _ -> (Nothing, [], createParserStatusError "error"
      ("error handle == " ++ show x) c l)
convertLDataToInstruct (LDataSymbol "<" _ : LDataGroup x _ : rest) env inst =
  case handleLDataGroup x rest env inst of
    (Just inst', env', _) -> (Just (init inst' ++ [Push (Op Less), Call, Ret]),
      env', createParserStatusOk)
    _ -> (Nothing, [], createParserStatusError "error" "error handle <" 0 0)
convertLDataToInstruct (LDataSymbol ">" _ : LDataGroup x _ : rest) env inst =
  case handleLDataGroup x rest env inst of
    (Just inst', env', _) -> (Just (init inst' ++ [Push (Op Greater),
      Call, Ret]), env', createParserStatusOk)
    _ -> (Nothing, [], createParserStatusError "error" "error handle >" 0 0)
convertLDataToInstruct (LDataSymbol "<=" _ : LDataGroup x _ : rest) env inst =
  case handleLDataGroup x rest env inst of
    (Just inst', env', _) -> (Just (init inst' ++ [Push (Op LessEq),Call, Ret]),
      env', createParserStatusOk)
    _ -> (Nothing, [], createParserStatusError "error" "error handle <=" 0 0)
convertLDataToInstruct (LDataSymbol ">=" _ : LDataGroup x _ : rest) env inst =
  case handleLDataGroup x rest env inst of
    (Just inst', env', _) -> (Just (init inst' ++
      [Push (Op GreaterEq), Call, Ret]), env', createParserStatusOk)
    _ -> (Nothing, [], createParserStatusError "error" "error handle >=" 0 0)
convertLDataToInstruct (LDataSymbol x (c, l) : _) _ _ =
  (Nothing, [], createParserStatusError "Error" 
    ("Error convertLDataToInstruct " ++ show x) c l)
convertLDataToInstruct (LDataInt x _ : rest) env inst =
  convertLDataToInstruct rest env (inst ++ [Push (Number x)])
convertLDataToInstruct (LDataFloat x _ : rest) env inst =
  convertLDataToInstruct rest env (inst ++ [Push (Float x)])
convertLDataToInstruct (LDataBool x _ : rest) env inst =
  convertLDataToInstruct rest env (inst ++ [Push (Boolean x)])
convertLDataToInstruct (LDataList x (_, _) : _) env inst =
  convertLDataToInstruct x env inst
convertLDataToInstruct (LDataDict x (_, _) : _) env inst =
  convertLDataToInstruct x env inst
convertLDataToInstruct (LDataTuple x (_, _) : _) env inst =
  convertLDataToInstruct x env inst
convertLDataToInstruct (LDataString x (_, _) : _) _ _ =
  (Just [Push (String x)], [], createParserStatusOk)
convertLDataToInstruct [] env inst =
  (Just (inst ++ [Ret]), env, createParserStatusOk)
convertLDataToInstruct _ _ _ = (Nothing, [], createParserStatusError "Error"
  "Error convertLDataToInstruct" 0 0)
