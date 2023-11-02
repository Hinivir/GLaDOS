
{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- TestInstruction.hs
-}


module TestInstruction (
    testHandleVar,
    testGetOp,
    testGetOp2
) where

import ParserStatus
import Vm (Instruction(..), Value(..), Operation(..))
import Parsing.LDataTree (LData(..))
import Parsing.Instruct.LDataToInstruct 
    (
    handleVar,
    getOp,
    getOp2,
    handleOpInt,
    handleOpFloat,
    handleGroupOpInt,
    handleIntOpGroup)

import Test.HUnit

testHandleVar :: Test
testHandleVar =
  TestList
    [
        "Test handleVar Int"
        ~: handleVar "foo" [LDataGroup [LDataInt 5 (0,0)](0,0)] [] []
        ~?= (Just [Ret], [("foo", Number 5)], createParserStatusOk),
        "Test handleVar Bool"
        ~: handleVar "foo" [LDataGroup [LDataBool True (0,0)](0,0)] [] []
        ~?= (Just [Ret], [("foo", Boolean True)], createParserStatusOk),
        "Test handleVar Float"
        ~: handleVar "foo" [LDataGroup [LDataFloat 5.2 (0,0)](0,0)] [] []
        ~?= (Just [Ret], [("foo", Float 5.2)], createParserStatusOk),
        "Test handleVar String"
        ~: handleVar "foo" [LDataGroup [LDataString "a" (0,0)](0,0)] [] []
        ~?= (Just [Ret], [("foo", String "a")], createParserStatusOk),
        "Test handleVar Error"
        ~: handleVar "foo" [LDataInt 5 (0,0)] [] []
        ~?= (Nothing, [], createParserStatusError "Error" "handleVar \"foo\"" 0 0)
    ]

testGetOp :: Test
testGetOp =
    TestList
        [
            "Test Op +"
            ~: getOp "+"
            ~?= Add,
            "Test Op -"
            ~: getOp "-"
            ~?= Sub,
            "Test Op *"
            ~: getOp "*"
            ~?= Mul,
            "Test Op div"
            ~: getOp "div"
            ~?= Div,
            "Test Op mod"
            ~: getOp "mod"
            ~?= Mod,
            "Test Op nothing"
            ~: getOp ">"
            ~?= Greater
        ]

testGetOp2 :: Test
testGetOp2 =
    TestList
        [
            "Test Op =="
            ~: getOp2 "=="
            ~?= Eq,
            "Test Op <"
            ~: getOp2 "<"
            ~?= Less,
            "Test Op >"
            ~: getOp2 ">"
            ~?= Greater,
            "Test Op <="
            ~: getOp2 "<="
            ~?= LessEq,
            "Test Op >="
            ~: getOp2 ">="
            ~?= GreaterEq
        ]

-- TODO Test handleLDataGroup
-- TODO Test handleOpInt
-- TODO Test handleGroupOpInt
-- TODO Test handleIntOpGroup
-- TODO Test handleOpFloat
-- TODO Test handleGroupOpFloat
-- TODO Test handleFloatOpGroup
-- TODO Test convertLDataToInstruct
