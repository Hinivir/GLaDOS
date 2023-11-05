{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- TestInstruction.hs
-}

module TestInstruction (
    testHandleVar,
    testGetArgs,
    testReplacePushEnv,
    testDetectPushEnvCall,
    testMoveBy,
    testGetOp,
    testGetOp2,
    testHandlLDataGroup,
    testHandleOpInt,
    testHandleGroupOpInt
) where

import ParserStatus (createParserStatusOk, createParserStatusError)
import Vm (Instruction(..), Value(..), Operation(..))
import Parsing.LDataTree (LData(..))
import Parsing.Instruct.LDataToInstruct
    (
    handleVar,
    getOp,
    getOp2,
    handleLDataGroup,
    handleOpInt,
    handleGroupOpInt,
    getArgs,
    replacePushEnv, detectPushEnvCall, moveBy
    )

import Test.HUnit

testHandleVar :: Test
testHandleVar =
  TestList
    [
        "Test handleVar Int"
        ~: handleVar "foo" [LDataGroup [LDataInt 5 (0,0)](0,0)] [] []
        ~?= (Just [], [("foo", Number 5)], createParserStatusOk),
        "Test handleVar Bool"
        ~: handleVar "foo" [LDataGroup [LDataBool True (0,0)](0,0)] [] []
        ~?= (Just [], [("foo", Boolean True)], createParserStatusOk),
        "Test handleVar Float"
        ~: handleVar "foo" [LDataGroup [LDataFloat 5.2 (0,0)](0,0)] [] []
        ~?= (Just [], [("foo", Float 5.2)], createParserStatusOk),
        "Test handleVar String"
        ~: handleVar "foo" [LDataGroup [LDataString "a" (0,0)](0,0)] [] []
        ~?= (Just [], [("foo", String "a")], createParserStatusOk),
        "Test handleVar Error"
        ~: handleVar "foo" [LDataInt 5 (0,0)] [] []
        ~?= (Nothing, [], createParserStatusError "Error" "handleVar \"foo\"" 0 0)
    ]

testGetArgs :: Test
testGetArgs =
    TestList
        [
            "Test getArgs args"
            ~: getArgs [LDataSymbol "x" (0,0), LDataSymbol ":" (0,0)] []
            ~?= ["x"],
            "Test getArgs args not empty"
            ~: getArgs [] ["y"]
            ~?= ["y"]
        ]

testReplacePushEnv :: Test
testReplacePushEnv =
    TestList
        [
            "Test replacePushEnv"
            ~: replacePushEnv ["x"] [PushEnv "x"]
            ~?= [PushArg 0],
            "Test replacePushEnv not in env"
            ~: replacePushEnv ["x"] [PushEnv "y"]
            ~?= [PushEnv "y"],
            "Test replacePushEnv not in env"
            ~: replacePushEnv [] [PushEnv "y"]
            ~?= [PushEnv "y"]
        ]

testDetectPushEnvCall :: Test
testDetectPushEnvCall =
    TestList
        [
            "Test detectPushEnvCall func"
            ~: detectPushEnvCall [Push (Number 5), PushEnv "x", Call] 0 [("x", Func [Push (Number 5)] 0)]
            ~?= (1, 2, 0),
            "Test detectPushEnvCall var"
            ~: detectPushEnvCall [PushEnv "x", Call] 0 [("x", Number 5)]
            ~?= (0, 0, 0),
            "Test detectPushEnvCall not in env"
            ~: detectPushEnvCall [PushEnv "x", Call] 0 []
            ~?= (0, 0, 0),
            "Test detectPushEnvCall no inst"
            ~: detectPushEnvCall [] 0 [("x", Func [Push (Number 5)] 0)]
            ~?= (0, 0, 0)
        ]

testMoveBy :: Test
testMoveBy = TestList [
    "test1"
    ~: moveBy [1, 2, 3, 4, 5] 1 2 
    ~?= [1, 4, 2, 3, 5],
    "test2"
    ~: moveBy [1, 2, 3, 4, 5] 0 3
    ~?= [3, 4, 1, 2, 5]
    ]

testGetOp :: Test
testGetOp =
    TestList
        [
            "Test Op +"
            ~: getOp "+"
            ~?= Just Add,
            "Test Op -"
            ~: getOp "-"
            ~?= Just Sub,
            "Test Op *"
            ~: getOp "*"
            ~?= Just Mul,
            "Test Op div"
            ~: getOp "div"
            ~?= Just Div,
            "Test Op mod"
            ~: getOp "mod"
            ~?= Just Mod,
            "Test Op nothing"
            ~: getOp ">"
            ~?= Just Greater
        ]

testGetOp2 :: Test
testGetOp2 =
    TestList
        [
            "Test Op =="
            ~: getOp2 "=="
            ~?= Just Eq,
            "Test Op <"
            ~: getOp2 "<"
            ~?= Just Less,
            "Test Op >"
            ~: getOp2 ">"
            ~?= Just Greater,
            "Test Op <="
            ~: getOp2 "<="
            ~?= Just LessEq,
            "Test Op >="
            ~: getOp2 ">="
            ~?= Just GreaterEq,
            "Test Op !="
            ~: getOp2 "!="
            ~?= Just Diff,
            "Test Nothing"
            ~: getOp2 " "
            ~?= Nothing
        ]

testHandlLDataGroup :: Test
testHandlLDataGroup =
    TestList
        [
            "Test Lipbe"
            ~: handleLDataGroup [LDataSymbol "Lipbe" (0,0), LDataSymbol "x" (0,0), LDataSymbol ":" (0,0)] [LDataGroup [LDataInt 5(0,0)](0,0)] [] []
            ~?= (Just [], [("x", Number 5)], createParserStatusOk),
            "Test Lipdo main"
            ~: handleLDataGroup [LDataSymbol "Lipdo" (0,0), LDataSymbol "main" (0,0), LDataSymbol ":" (0,0)] [LDataGroup [LDataInt 5 (0,0), LDataSymbol "+" (0,0), LDataInt 5 (0,0)](0,0)] [] []
            ~?= (Just [Push (Number 5), Push (Number 5), Push (Op Add), Call], [], createParserStatusOk),
            "Test Var Op Int"
            ~: handleLDataGroup [LDataSymbol "x" (0,0), LDataSymbol "+" (0,0), LDataInt 5 (0,0)] [] [("x", Number 5)] []
            ~?= (Just [PushEnv "x", Push (Number 5), Push (Op Add), Call], [("x", Number 5)], createParserStatusOk),
            "Test Var Op Float"
            ~: handleLDataGroup [LDataSymbol "x" (0,0), LDataSymbol "+" (0,0), LDataFloat 5.2 (0,0)] [] [("x", Number 5)] []
            ~?= (Just [PushEnv "x", Push (Float 5.2), Push (Op Add), Call], [("x", Number 5)], createParserStatusOk),
            "Test Int Op Var"
            ~: handleLDataGroup [LDataInt 5 (0,0), LDataSymbol "+" (0,0), LDataSymbol "x" (0,0)] [] [("x", Number 5)] []
            ~?= (Just [Push (Number 5), PushEnv "x", Push (Op Add), Call], [("x", Number 5)], createParserStatusOk),
            "Test Float Op Var"
            ~: handleLDataGroup [LDataFloat 5.2 (0,0), LDataSymbol "+" (0,0), LDataSymbol "x" (0,0)] [] [("x", Number 5)] []
            ~?= (Just [Push (Float 5.2), PushEnv "x", Push (Op Add), Call], [("x", Number 5)], createParserStatusOk),
            "Test handle group"
            ~: handleLDataGroup [LDataGroup [LDataInt 5 (0,0), LDataSymbol "+" (0,0), LDataInt 5 (0,0)] (0,0), LDataInt 5 (0,0)] [LDataFloat 5.2 (0,0)] [] []
            ~?= (Just [Push (Number 5), Push (Number 5), Push (Op Add), Call, Push (Number 5), Push (Float 5.2)], [], createParserStatusOk),
            "Test handle Symbol"
            ~: handleLDataGroup [LDataSymbol "x" (0,0)] [LDataInt 6 (0,0), LDataSymbol "+" (0,0), LDataInt 5 (0,0)] [("x", Number 5)] []
            ~?= (Just [PushEnv "x", Push (Number 6), Push (Number 5),  Push (Op Add), Call], [("x", Number 5)], createParserStatusOk),
            "Test Int Op Group"
            ~: handleLDataGroup [LDataInt 5 (0,0), LDataSymbol "+" (0,0), LDataGroup [LDataInt 5 (0,0), LDataSymbol "+" (0,0), LDataInt 5 (0,0)] (0,0)] [] [] []
            ~?= (Just [Push (Number 5), Push (Number 5), Push (Op Add), Call, Push (Number 5), Push (Op Add), Call], [], createParserStatusOk),
            "Test Float Op Float"
            ~: handleLDataGroup [LDataFloat 5.2 (0,0), LDataSymbol "+" (0,0), LDataFloat 5.2 (0,0)] [] [] []
            ~?= (Just [Push (Float 5.2), Push (Float 5.2), Push (Op Add), Call], [], createParserStatusOk),
            "Test Float Op Group"
            ~: handleLDataGroup [LDataFloat 5.2 (0,0), LDataSymbol "+" (0,0), LDataGroup [LDataInt 5 (0,0), LDataSymbol "+" (0,0), LDataInt 5 (0,0)] (0,0)] [] [] []
            ~?= (Just [Push (Number 5), Push (Number 5), Push (Op Add), Call, Push (Float 5.2), Push (Op Add), Call], [], createParserStatusOk),
            "Test []"
            ~: handleLDataGroup [] [] [] []
            ~?= (Just [], [], createParserStatusOk),
            "Test x []"
            ~: handleLDataGroup [] [LDataInt 5 (0,0)] [] []
            ~?= (Just [Push (Number 5)], [], createParserStatusOk)
        ]

testHandleOpInt :: Test
testHandleOpInt = TestList
    [
        "handleOpInt"
        ~: handleOpInt 5 "+" 5 [] [] []
        ~?= (Just [Push (Number 5), Push (Number 5), Push (Op Add), Call], [], createParserStatusOk)
    ]

-- TODO: Test handleGroupOpInt

testHandleGroupOpInt :: Test
testHandleGroupOpInt = TestList
    [
        "Test work handleGroupOpInt"
        ~: handleGroupOpInt [LDataGroup [LDataInt 5 (0,0), LDataSymbol "+" (0,0), LDataInt 5 (0,0)] (0,0)] "+" 5 [] [] []
        ~?= (Just [Push (Number 5), Push (Number 5), Push (Op Add), Call, Push (Number 5), Push (Op Add), Call], [], createParserStatusOk)
    ]

-- TODO: Test handleIntOpGroup
-- TODO: Test handleOpFloat
-- TODO: Test handleGroupOpFloat
-- TODO: Test handleFloatOpGroup
-- TODO: Test convertLDataToInstruct
