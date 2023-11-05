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
    getOp2,
    getArgs,
    replacePushEnv,
    detectPushEnvCall,
    moveBy
  ) where

import ParserStatus
  (
    createParserStatusOk,
    createParserStatusError,
    ParserStatus,
    interpretParserStatus
  )

import Vm
  (
    Operation(..),
    Instructions,
    Value(..),
    Env,
    Instruction(..),
    Builtin(..)
  )

import Parsing.LDataTree
  (
    LData(..),
  )

import Data.List (elemIndex)
import Data.Maybe (fromJust)

handleVar :: String -> [LData] -> Env -> Instructions -> (Maybe Instructions, Env, ParserStatus)
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

getArgs :: [LData] -> [String] -> [String]
getArgs (LDataSymbol ":" _ : _) args = args
getArgs (LDataSymbol x _ : rest) args = getArgs rest (args ++ [x])
getArgs _ args = args

replacePushEnv :: [String] -> Instructions -> Instructions
replacePushEnv envStrings = map replaceIfPushEnv 
  where
    replaceIfPushEnv (PushEnv x) =
      case elemIndex x envStrings of
        Just nb -> PushArg nb
        Nothing -> PushEnv x
    replaceIfPushEnv instr = instr

detectPushEnvCall :: Instructions -> Int -> Env -> (Int, Int, Int)
detectPushEnvCall (PushEnv x : Call : _) count env =
    case lookupVarOrFunc x env of
        Just (Right (_, nb)) -> (count, count + 1, nb)
        Just  (Left _) -> (0, 0, 0)
        Nothing -> (0, 0, 0)
detectPushEnvCall (_ : rest) count env = detectPushEnvCall rest (count + 1) env
detectPushEnvCall [] _ _ = (0, 0, 0)

moveBy :: [a] -> Int -> Int -> [a]
moveBy list index positions
    | index < 0 || index >= length list - 1 = error "Invalid index"
    | otherwise = let (xs, ys) = splitAt index list
                      (zs, ws) = splitAt 2 ys
                      (as, bs) = splitAt (positions - 1) ws
                  in xs ++ as ++ zs ++ bs

getLipdo :: [LData] -> Int -> Int
getLipdo (LDataSymbol "Lipdo" _: _) pos = pos - 1
getLipdo (_ : rest) pos = getLipdo rest (pos + 1)
getLipdo _ pos = pos - 1

getMove :: Instructions -> Int -> Int
getMove (Call : _) nb = nb
getMove (_ : rest) nb = getMove rest (nb + 1)
getMove _ nb = nb

findPushEnvCall :: String -> Instructions -> Int -> Int
findPushEnvCall name (PushEnv x : Call : _) nb
  | x == name = nb
findPushEnvCall name (_ : rest) nb = findPushEnvCall name rest (nb + 1)
findPushEnvCall _ [] _ = 0

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex _ _ [] = []
replaceAtIndex 0 newElem (_:xs) = newElem : xs
replaceAtIndex i newElem (x:xs) = x : replaceAtIndex (i - 1) newElem xs

findCall :: Instructions -> Int -> Int
findCall [] nb = nb
findCall (Call : rest) nb = findCall rest (nb + 1)
findCall (_ : rest) nb = findCall rest nb


findJumpOrJumpIfFalse :: Instructions -> Int -> Either (Int, Int) (Int, Int)
findJumpOrJumpIfFalse (Jump x : _) nb = Left (nb, x)
findJumpOrJumpIfFalse (JumpIfFalse x : _) nb = Right (nb, x)
findJumpOrJumpIfFalse (_ : rest) nb = findJumpOrJumpIfFalse rest (nb + 1)
findJumpOrJumpIfFalse [] _ = Left (0, 0)

detectAndMove :: Instructions -> Int -> String -> Instructions
detectAndMove x y name = case findPushEnvCall name x 0 of
  0 -> x
  pos -> case moveBy x pos (getMove (drop pos x) 0 + y + 3) of
    [] -> error "Error"
    x' -> case findJumpOrJumpIfFalse (drop (pos - 2) x') 0 of
      Left (nb, z) -> replaceAtIndex (nb + pos - 2) (Jump (2 + z + findCall
        (drop (pos - 2) x') 0)) x'
      Right (nb, z) -> replaceAtIndex (nb + pos - 2) (JumpIfFalse (2 + z +
        findCall (drop (pos - 2) x') 0)) x'

insertAt :: Int -> a -> [a] -> [a]
insertAt index element list = before ++ [element] ++ after
  where (before, after) = splitAt index list

findName :: String -> Instructions -> Instructions -> Int -> Instructions
findName name (PushEnv x : _) inst nb
  | name == x = insertAt (nb + 1) Call inst
findName name (_ : rest) inst nb = findName name rest inst (nb + 1)
findName _ _ inst _ = inst

handleFunction :: String -> [String] -> [LData] -> Env -> Instructions -> (Maybe Instructions, Env, ParserStatus)
handleFunction name args (LDataGroup x _ : rest) env inst =
    let (func, z) = splitAt (getLipdo rest 0) rest
    in case handleLDataGroup x func env inst of
        (Nothing, _, err) -> (Nothing, [], createParserStatusError "Error"
            (interpretParserStatus err) 0 0)
        (Just res, newEnv, _) -> convertLDataToInstruct z ((name, Func
          (detectAndMove (findName name (replacePushEnv args res)
          (replacePushEnv args res) 0) (length args) name ++ [Ret])
          (length args)) : newEnv) inst 
handleFunction _ _ _ _ _ = (Nothing, [], createParserStatusError "Error"
    "handleFunction" 0 0)

getOpInGroup :: [LData] -> Int -> Int
getOpInGroup (LDataSymbol op _ : rest) nb = case getOp op of
    Just _ -> getOpInGroup rest (nb + 1)
    Nothing -> getOpInGroup rest nb
getOpInGroup []  nb = nb
getOpInGroup _ nb = nb

getLength :: [LData] -> Int
getLength x = (length x + 1) + getOpInGroup x 0

getLength' :: [LData] -> Int
getLength' x = length x + getOpInGroup x 0

handleIf :: [LData] -> [LData] -> Env -> Instructions -> (Maybe Instructions, Env, ParserStatus)
handleIf x (LDataGroup y _ : LDataGroup [LDataSymbol "else" _] _ :
    LDataGroup z _ : rest) env inst = case convertLDataToInstruct x env inst of
        (Just x', env', _) -> case convertLDataToInstruct y env' (x' ++
            [JumpIfFalse (getLength y)]) of
            (Just y', env'', _) -> handleIf' z rest y' env''
            (Nothing, _, err) -> (Nothing, [], createParserStatusError
                "Error" (interpretParserStatus err) 0 0)
        (Nothing, _, err) -> (Nothing, [], createParserStatusError
            "Error" (interpretParserStatus err) 0 0)
handleIf x (LDataGroup y _  : rest) env inst =
    case convertLDataToInstruct x env inst of
        (Just x', env', _) -> case convertLDataToInstruct y env' (x' ++
            [JumpIfFalse (getLength' y)]) of
              (Just y', env'', _) -> convertLDataToInstruct rest env'' y'
              (Nothing, _, err) -> (Nothing, [], createParserStatusError
                "Error" (interpretParserStatus err) 0 0)
        (Nothing, _, err) -> (Nothing, [], createParserStatusError "Error"
            (interpretParserStatus err) 0 0)
handleIf _ _ _ _ = (Nothing, [], createParserStatusError "Error""HandleIf" 0 0)

handleIf' :: [LData] -> [LData] -> Instructions -> Env -> (Maybe Instructions, Env, ParserStatus)
handleIf' z rest y' env'' = case convertLDataToInstruct z env'' (y' ++
                [Jump (getLength' z)]) of
                (Just z', env''', _) -> convertLDataToInstruct rest env''' z'
                (Nothing, _, err) -> (Nothing, [], createParserStatusError
                    "Error" (interpretParserStatus err) 0 0)

getOp :: String -> Maybe Operation
getOp x = case x of
  "+" -> Just Add
  "-" -> Just Sub
  "*" -> Just Mul
  "div" -> Just Div
  "mod" -> Just Mod
  _ -> getOp2 x

getOp2 :: String -> Maybe Operation
getOp2 x = case x of
  "==" -> Just Eq
  "<" -> Just Less
  ">" -> Just Greater
  "<=" -> Just LessEq
  ">=" -> Just GreaterEq
  "!=" -> Just Diff
  _ -> Nothing

lookupVarOrFunc :: String -> Env -> Maybe (Either String (String, Int))
lookupVarOrFunc name env = case lookup name env of
  Just (Func _ nb) -> Just (Right (name, nb))
  Just _ -> Just (Left name)
  Nothing -> Nothing   

listToValue :: [LData] -> [Value] -> [Value]
listToValue (LDataInt x _ : rest) val = listToValue rest (val ++ [Number x])
listToValue (LDataSymbol x _ : rest) val = listToValue rest (val ++ [String x])
listToValue (LDataBool x _ : rest) val = listToValue rest (val ++ [Boolean x])
listToValue (LDataFloat x _ : rest) val = listToValue rest (val ++ [Float x])
listToValue [] val = val
listToValue _ _ = []

handleLDataGroup :: [LData] -> [LData] -> Env -> Instructions -> (Maybe Instructions, Env, ParserStatus)
handleLDataGroup (LDataSymbol "Lipbe" _ : LDataSymbol x _ : LDataSymbol ":" _ :
  _) group env inst = handleVar x group env inst
handleLDataGroup (LDataSymbol "Lipdo" _ : LDataSymbol "main" _ : _)
  y env inst = convertLDataToInstruct y env inst
handleLDataGroup (LDataSymbol "Lipdo" _ : LDataSymbol name _ : args)
  y env inst = handleFunction name (getArgs args []) y env inst
handleLDataGroup (LDataSymbol "if" _ : LDataGroup x _ : z) y env inst =
    handleIf x (y ++ z) env inst 
handleLDataGroup (LDataSymbol x _ : LDataSymbol op _ : LDataInt y _ : rest)
  z env inst = convertLDataToInstruct (rest ++ z) env (inst ++ [PushEnv x,
    Push (Number y), Push (Op (fromJust (getOp op))), Call])
handleLDataGroup (LDataSymbol x _ : LDataSymbol op _ : LDataFloat y _ : rest)
  z env inst = convertLDataToInstruct (rest ++ z) env (inst ++ [PushEnv x,
    Push (Float y), Push (Op (fromJust (getOp op))), Call])
handleLDataGroup (LDataInt x _ : LDataSymbol op _ : LDataSymbol y _ : rest)
  z env inst = convertLDataToInstruct (rest ++ z) env (inst ++ [Push
  (Number x), PushEnv y, Push (Op (fromJust (getOp op))), Call])
handleLDataGroup (LDataFloat x _ : LDataSymbol op _ : LDataSymbol y _ : rest)
  z env inst = convertLDataToInstruct (rest ++ z) env (inst ++ [Push (Float x),
  PushEnv y, Push (Op (fromJust (getOp op))), Call])
handleLDataGroup (LDataGroup x _ : rest) y env inst =
  handleLDataGroup x (rest ++ y) env inst
handleLDataGroup (LDataInt x _ : LDataSymbol op _ : LDataInt y' _ : rest)
  y env inst = handleOpInt x op y' (rest ++ y) env inst
handleLDataGroup (LDataSymbol x _ : LDataSymbol op _ : LDataSymbol y _ : rest)
  z env inst = convertLDataToInstruct (rest ++ z) env (inst ++ [PushEnv x,
    PushEnv y, Push (Op (fromJust (getOp op))), Call])
handleLDataGroup (LDataSymbol x _ : rest) y env inst =
  case lookupVarOrFunc x env of
  Just (Left name) ->
    convertLDataToInstruct (rest ++ y) env (inst ++ [PushEnv name])
  Just (Right (name', _)) ->
    convertLDataToInstruct (rest ++ y) env (inst ++ [PushEnv name', Call])
  Just _ ->  convertLDataToInstruct (rest ++ y) env (inst ++ [PushEnv x])
  Nothing -> convertLDataToInstruct (rest ++ y) env (inst ++ [PushEnv x])
handleLDataGroup (LDataInt x _ : LDataSymbol op _ : LDataGroup y _ : rest)
  z env inst = handleIntOpGroup x op y (rest ++ z) env inst
handleLDataGroup (LDataFloat x _ : LDataSymbol op _ : LDataFloat y' _ : rest)
  y env inst = handleOpFloat x op y' (rest ++ y) env inst
handleLDataGroup (LDataFloat x _ : LDataSymbol op _ : LDataGroup y _ : rest)
  z env inst = handleFloatOpGroup x op y (rest ++ z) env inst
handleLDataGroup (LDataInt x _ : rest) y env inst =
    convertLDataToInstruct (rest ++ y) env (inst ++ [Push (Number x)])
handleLDataGroup (LDataFloat x _ : rest) y env inst =
    convertLDataToInstruct (rest ++ y) env (inst ++ [Push (Float x)])
handleLDataGroup [] [] env inst = convertLDataToInstruct [] env inst
handleLDataGroup [] x env inst = convertLDataToInstruct x env inst
handleLDataGroup x _ _ _ =
  (Nothing, [], createParserStatusError "Error" ("handleLDataGroup " ++ show x)
    0 0)

handleOpInt :: Int -> String -> Int -> [LData] -> Env -> Instructions -> (Maybe Instructions, Env, ParserStatus)
handleOpInt x op y rest env inst =
  convertLDataToInstruct rest env (inst ++ [Push (Number x), Push (Number y),
    Push (Op (fromJust (getOp op))), Call])

handleGroupOpInt :: [LData] -> String -> Int -> [LData] -> Env -> Instructions -> (Maybe Instructions, Env, ParserStatus)
handleGroupOpInt x op y rest env inst =
  case handleLDataGroup x rest env inst of
    (Nothing, _, _) -> (Nothing, [], createParserStatusError "error"
      ("error handle " ++ op) 0 0)
    (Just inst', env', _) -> (Just ( inst' ++ [Push (Number y),
      Push (Op (fromJust (getOp op))), Call]), env', createParserStatusOk)

handleIntOpGroup :: Int -> String -> [LData] -> [LData] -> Env -> Instructions -> (Maybe Instructions, Env, ParserStatus)
handleIntOpGroup x op y rest env inst =
  case handleLDataGroup y rest env inst of
    (Nothing, _, _) -> (Nothing, [], createParserStatusError "error"
      ("error handle " ++ op) 0 0)
    (Just inst', env', _) -> (Just ( inst' ++ [Push (Number x),
      Push (Op (fromJust (getOp op))), Call]), env', createParserStatusOk)

handleOpFloat :: Float -> String -> Float -> [LData] -> Env -> Instructions -> (Maybe Instructions, Env, ParserStatus)
handleOpFloat x op y rest env inst =
  convertLDataToInstruct rest env (inst ++ [Push (Float x), Push (Float y),
    Push (Op (fromJust (getOp op))), Call])

handleGroupOpFloat :: [LData] -> String -> Float -> [LData] -> Env -> Instructions -> (Maybe Instructions, Env, ParserStatus)
handleGroupOpFloat x op y rest env inst =
  case handleLDataGroup x rest env inst of
    (Nothing, _, _) -> (Nothing, [], createParserStatusError "error"
      ("error handle " ++ op) 0 0)
    (Just inst', env', _) -> (Just ( inst' ++ [Push (Float y),
      Push (Op (fromJust (getOp op))), Call]), env', createParserStatusOk)

handleFloatOpGroup :: Float -> String -> [LData] -> [LData] -> Env -> Instructions -> (Maybe Instructions, Env, ParserStatus)
handleFloatOpGroup x op y rest env inst =
  case handleLDataGroup y rest env inst of
    (Nothing, _, _) -> (Nothing, [], createParserStatusError "error"
      ("error handle " ++ op) 0 0)
    (Just inst', env', _) -> (Just ( inst' ++ [Push (Float x),
      Push (Op (fromJust (getOp op))), Call]), env', createParserStatusOk)

convertLDataToInstruct :: [LData] -> Env -> Instructions -> (Maybe Instructions, Env, ParserStatus)
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
    (Just inst', env', _) -> (Just ( inst' ++ [Push (Op Add), Call]),
      env', createParserStatusOk)
    _ -> (Nothing, [], createParserStatusError "error" "error handle +" 0 0)
convertLDataToInstruct (LDataSymbol "-" _ : LDataGroup x _ : rest) env inst =
  case handleLDataGroup x rest env inst of
    (Just inst', env', _) -> (Just ( inst' ++ [Push (Op Sub), Call]),
      env', createParserStatusOk)
    _ -> (Nothing, [], createParserStatusError "error" "error handle -" 0 0)
convertLDataToInstruct (LDataSymbol "*" _ : LDataGroup x _ : rest) env inst =
  case handleLDataGroup x rest env inst of
    (Just inst', env', _) -> (Just ( inst' ++ [Push (Op Mul), Call]),
      env', createParserStatusOk)
    _ -> (Nothing, [], createParserStatusError "error" "error handle *" 0 0)
convertLDataToInstruct (LDataSymbol "div" _ : LDataGroup x _ : rest) env inst =
  case handleLDataGroup x rest env inst of
    (Just inst', env', _) -> (Just ( inst' ++ [Push (Op Div), Call]),
      env', createParserStatusOk)
    _ -> (Nothing, [], createParserStatusError "error" "error handle div" 0 0)
convertLDataToInstruct (LDataSymbol "mod" _ : LDataGroup x _ : rest) env inst =
  case handleLDataGroup x rest env inst of
    (Just inst', env', _) -> (Just ( inst' ++ [Push (Op Mod), Call]),
      env', createParserStatusOk)
    _ -> (Nothing, [], createParserStatusError "error" "error handle mod" 0 0)
convertLDataToInstruct (LDataSymbol "==" (l, c) : LDataGroup x _ : rest)
  env inst = case handleLDataGroup x rest env inst of
    (Just inst', env', _) -> (Just ( inst' ++ [Push (Op Eq), Call]),
      env', createParserStatusOk)
    _ -> (Nothing, [], createParserStatusError "error"
      ("error handle == " ++ show x) c l)
convertLDataToInstruct (LDataSymbol "<" _ : LDataGroup x _ : rest) env inst =
  case handleLDataGroup x rest env inst of
    (Just inst', env', _) -> (Just ( inst' ++ [Push (Op Less), Call]),
      env', createParserStatusOk)
    _ -> (Nothing, [], createParserStatusError "error" "error handle <" 0 0)
convertLDataToInstruct (LDataSymbol ">" _ : LDataGroup x _ : rest) env inst =
  case handleLDataGroup x rest env inst of
    (Just inst', env', _) -> (Just ( inst' ++ [Push (Op Greater),
      Call]), env', createParserStatusOk)
    _ -> (Nothing, [], createParserStatusError "error" "error handle >" 0 0)
convertLDataToInstruct (LDataSymbol "<=" _ : LDataGroup x _ : rest) env inst =
  case handleLDataGroup x rest env inst of
    (Just inst', env', _) -> (Just ( inst' ++ [Push (Op LessEq),Call]),
      env', createParserStatusOk)
    _ -> (Nothing, [], createParserStatusError "error" "error handle <=" 0 0)
convertLDataToInstruct (LDataSymbol ">=" _ : LDataGroup x _ : rest) env inst =
  case handleLDataGroup x rest env inst of
    (Just inst', env', _) -> (Just ( inst' ++
      [Push (Op GreaterEq), Call]), env', createParserStatusOk)
    _ -> (Nothing, [], createParserStatusError "error" "error handle >=" 0 0)
convertLDataToInstruct (LDataSymbol "len" _ : LDataList x _ : rest) env inst =
    convertLDataToInstruct rest env (inst ++ [Push (List (listToValue x [])),
        Push (Builtin Len), Call])

convertLDataToInstruct (LDataSymbol "head" _ : LDataList x _ : rest) env inst =
    convertLDataToInstruct rest env (inst ++ [Push (List (listToValue x [])),
        Push (Builtin Head), Call])
convertLDataToInstruct (LDataSymbol "tail" _ : LDataList x _ : rest) env inst =
    convertLDataToInstruct rest env (inst ++ [Push (List (listToValue x [])),
        Push (Builtin Tail), Call])
convertLDataToInstruct (LDataSymbol "True" _ : rest) env inst =
    convertLDataToInstruct rest env (inst ++ [Push (Boolean True)])
convertLDataToInstruct (LDataSymbol "False" _ : rest) env inst =
    convertLDataToInstruct rest env (inst ++ [Push (Boolean False)])
convertLDataToInstruct (LDataSymbol x _ : LDataSymbol op _ : LDataInt y _ :
  rest) env inst = convertLDataToInstruct rest env (inst ++ [PushEnv x,
    Push (Number y), Push (Op (fromJust (getOp op))), Call])
convertLDataToInstruct (LDataSymbol x _ : LDataSymbol op _ : LDataFloat y _ :
  rest) env inst = convertLDataToInstruct rest env (inst ++ [PushEnv x,
    Push (Float y), Push (Op (fromJust (getOp op))), Call])
convertLDataToInstruct (LDataInt x _ : LDataSymbol op _ : LDataSymbol y _ :
  rest) env inst = convertLDataToInstruct rest env (inst ++ [Push
  (Number x), PushEnv y, Push (Op (fromJust (getOp op))), Call])
convertLDataToInstruct (LDataFloat x _ : LDataSymbol op _ : LDataSymbol y _ :
  rest) env inst = convertLDataToInstruct rest env (inst ++ [Push (Float x),
  PushEnv y, Push (Op (fromJust (getOp op))), Call])
convertLDataToInstruct (LDataSymbol x _ : LDataSymbol op _ : LDataSymbol y _ :
  rest) env inst = convertLDataToInstruct rest env (inst ++ [PushEnv x,
    PushEnv y, Push (Op (fromJust (getOp op))), Call])
convertLDataToInstruct (LDataSymbol x _ : rest) env inst =
  case lookupVarOrFunc x env of
    Just (Left name) ->
        convertLDataToInstruct rest env (inst ++ [PushEnv name])
    Just (Right (name', _)) ->
        convertLDataToInstruct rest env (inst ++ [PushEnv name', Call])
    Nothing -> convertLDataToInstruct rest env (inst ++ [PushEnv x])
convertLDataToInstruct (LDataInt x _ : rest) env inst =
  convertLDataToInstruct rest env (inst ++ [Push (Number x)])
convertLDataToInstruct (LDataFloat x _ : rest) env inst =
  convertLDataToInstruct rest env (inst ++ [Push (Float x)])
convertLDataToInstruct (LDataBool x _ : rest) env inst =
  convertLDataToInstruct rest env (inst ++ [Push (Boolean x)])
convertLDataToInstruct (LDataList x _ : rest) env inst =
    convertLDataToInstruct rest env (inst ++ [Push (List (listToValue x []))])
convertLDataToInstruct (LDataDict x (_, _) : _) env inst =
  convertLDataToInstruct x env inst
convertLDataToInstruct (LDataTuple x (_, _) : _) env inst =
  convertLDataToInstruct x env inst
convertLDataToInstruct (LDataString x (_, _) : _) _ _ =
  (Just [Push (String x)], [], createParserStatusOk)
convertLDataToInstruct [] env inst =
  (Just inst, env, createParserStatusOk)
convertLDataToInstruct _ _ _ = (Nothing, [], createParserStatusError "Error"
  "Error convertLDataToInstruct" 0 0)
