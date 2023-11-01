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


import Vm
  (
    Value(..),
    Operation(..),
    Instruction(..),
    Instructions,
    Env
  )
--import Parsing.Instruct
--  (
--    Value(..),
--    Operation(..),
--    Instruction(..),
--    Instructions,
--    Env
--  )

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
--handleVar x (LDataGroup [LDataString y _] _ : z) env inst =
--  convertLDataToInstruct z ((x, [String] y) : env) inst
handleVar _ x _ _ = (Nothing, [], createParserStatusError "Error"
  ("handleVar " ++ (show x)) 0 0)

handleLDataGroup :: [LData] -> [LData] -> Env -> Instructions ->
  (Maybe Instructions, Env, ParserStatus)
handleLDataGroup (LDataSymbol "Lipbe" (_, _) : LDataSymbol x (_, _)
  : LDataSymbol ":" (_, _) : _) group env inst =
  handleVar x group env inst
handleLDataGroup(LDataSymbol "Lipdo" _ : LDataSymbol "main" _ : _) y env inst =
  convertLDataToInstruct y env inst
handleLDataGroup (LDataInt x _ : LDataSymbol "+" _ :
  LDataInt y' _ : rest) y env inst =
  convertLDataToInstruct (rest ++ y) env (inst ++ [Push (Number x),
  Push (Number y'), Push (Op Add), Call])
handleLDataGroup (LDataGroup [LDataInt x _, LDataSymbol "-" _, LDataInt y' _] _
  : rest) y env inst =
  convertLDataToInstruct (rest ++ y) env (inst ++ [Push (Number x),
  Push (Number y'), Push (Op Sub), Call])
handleLDataGroup (LDataGroup [LDataInt x _, LDataSymbol "*" _, LDataInt y' _] _
  : rest) y env inst =
  convertLDataToInstruct (rest ++ y) env (inst ++ [Push (Number x),
  Push (Number y'), Push (Op Mul), Call])
handleLDataGroup (LDataInt x _ : LDataSymbol "div" _ : LDataInt y' _
  : rest) y env inst =
  convertLDataToInstruct (rest ++ y) env (inst ++ [Push (Number x),
  Push (Number y'), Push (Op Div), Call])
handleLDataGroup (LDataInt x _ : LDataSymbol "==" _ : LDataInt y' _ : rest)
  y env inst =
  convertLDataToInstruct (rest ++ y) env (inst ++ [Push (Number x),
  Push (Number y'), Push (Op Eq), Call])
handleLDataGroup (LDataInt x _ : LDataSymbol "<" _ : LDataInt y' _ : rest)
  y env inst =
  convertLDataToInstruct (rest ++ y) env (inst ++ [Push (Number x),
  Push (Number y'), Push (Op Less), Call])
handleLDataGroup (LDataSymbol x _ : rest) y env inst =
  convertLDataToInstruct (rest ++ y) env (inst ++ [PushEnv x])
handleLDataGroup (LDataInt x _ : LDataSymbol "+" _ : LDataSymbol y _ : rest)
  z env inst =
  convertLDataToInstruct (rest ++ z) env (inst ++ [Push (Number x),
  PushEnv y, Push (Op Add), Call])
handleLDataGroup (LDataInt x _ : LDataSymbol "-" _ : LDataSymbol y _ : rest)
  z env inst =
  convertLDataToInstruct (rest ++ z) env (inst ++ [Push (Number x), PushEnv y,
  Push (Op Sub), Call])
handleLDataGroup (LDataInt x _ : LDataSymbol "*" _ : LDataSymbol y _ : rest)
  z env inst =
  convertLDataToInstruct (rest ++ z) env (inst ++ [Push (Number x), PushEnv y,
  Push (Op Mul), Call])
handleLDataGroup (LDataInt x _ : LDataSymbol "div" _ : LDataSymbol y _ : rest)
  z env inst =
  convertLDataToInstruct (rest ++ z) env (inst ++ [Push (Number x), PushEnv y,
  Push (Op Div), Call])
handleLDataGroup (LDataInt x _ : LDataSymbol "==" _ : LDataSymbol y _ : rest)
  z env inst =
  convertLDataToInstruct (rest ++ z) env (inst ++ [Push (Number x), PushEnv y,
  Push (Op Eq), Call])
handleLDataGroup (LDataInt x _ : LDataSymbol "<" _ : LDataSymbol y _ : rest)
  z env inst =
  convertLDataToInstruct (rest ++ z) env (inst ++ [Push (Number x), PushEnv y,
  Push (Op Less), Call])
handleLDataGroup [] x env inst = convertLDataToInstruct x env inst
handleLDataGroup _ _ _ _ = (Nothing, [], createParserStatusError "Error"
  "handleLDataGroup" 0 0)

convertLDataToInstruct :: [LData] -> Env -> Instructions ->
  (Maybe Instructions, Env, ParserStatus)
convertLDataToInstruct (LDataGroup x (_, _): y) env inst =
  handleLDataGroup x y env inst
convertLDataToInstruct (LDataInt x (_, _) : _) env inst =
  (Just (inst ++ [Push (Number x)]), env, createParserStatusOk)
convertLDataToInstruct (LDataFloat x (_, _) : _) env _ =
  (Just [Push (Float x)], env, createParserStatusOk)
convertLDataToInstruct (LDataBool x (_, _) : _) env _ =
  (Just [Push (Boolean x)], env, createParserStatusOk)
convertLDataToInstruct (LDataList x (_, _) : _) env inst =
  convertLDataToInstruct x env inst
convertLDataToInstruct (LDataDict x (_, _) : _) env inst =
  convertLDataToInstruct x env inst
convertLDataToInstruct (LDataTuple x (_, _) : _) env inst =
  convertLDataToInstruct x env inst
convertLDataToInstruct (LDataSymbol x (l, c): _) _  _ =
  (Nothing, [], createParserStatusError "Error"
  ("Symbol Not Know " ++ x) l c)
--convertLDataToInstruct (LDataString x (_, _) : _) _ _ =
--  (Just [Push (String x)], [], createParserStatusOk)
convertLDataToInstruct [] env inst =
  (Just (inst ++ [Ret]), env, createParserStatusOk)
convertLDataToInstruct (LDataUndefined : _) _ _ =
  (Nothing, [], createParserStatusError "Error"
  "Can't Convert Undefined" 0 0)

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

=>

Env:
[(add, [Push a, Push Op Add, Push b])]

Instructions:
[Push 3, Push 4, Pushenv add, Call, Ret]


---------------------------

[LDataGroup [LDataSymbol "Lipbe" (1,1),
LDataSymbol "foo" (7,1),LDataSymbol ":" (10,1)] (0,0),
LDataGroup [LDataInt 42 (3,2)] (0,0),
LDataGroup [LDataSymbol "Lipdo" (1,3),
LDataSymbol "main" (7,3),
LDataSymbol ":" (11,3)] (0,0),
LDataGroup [LDataSymbol "if" (3,4),
  LDataGroup [LDataSymbol "foo" (7,4),
    LDataSymbol "<" (11,4),
    LDataInt 10 (13,4)] (6,4)] (0,0),
  LDataGroup [LDataSymbol "foo" (5,5),
    LDataSymbol "*" (9,5),LDataInt 3 (11,5)] (0,0),
  LDataGroup [LDataSymbol "else" (3,6)] (0,0),
  LDataGroup [LDataSymbol "foo" (5,7),
    LDataSymbol "/" (9,7),
    LDataInt 2 (11,7)] (0,0)]

=>

Env:
[(foo, 42)]

Instructions:
[If [PushEnv foo, Push 10, Push Op Less], [PushEnv foo, Push 3, Push Op Mul], [PushEnv foo, Push 2, Push Op Div], Ret]

--------------------------------

[LDataGroup 
  [LDataSymbol "Lipdo" (1,1),
  LDataSymbol "main" (7,1),LDataSymbol ":" (11,1)] (0,0),
LDataGroup 
  [LDataGroup
    [LDataInt 2 (6,2),
    LDataSymbol "*" (8,2),
    LDataInt 3 (10,2)
  ](5,2),
  LDataSymbol "+" (13,2),
  LDataGroup [
    LDataInt 10 (16,2),
    LDataSymbol "div" (19,2),
    LDataInt 2 (23,2)
    ] (15,2)
  ] (0,0)
]

=>

Env:
[]

Instructions:
[Push 2, Push 3, Push Op Mul, Call, Push 10, Push 2, Push Op Div, Call, Push Op Add, Call, Ret]


[Push (Number 2),Push (Number 3),Push (Op Add),Call,Ret]
[]


--}
