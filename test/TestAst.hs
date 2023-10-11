{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- test ast
-}


module TestAst (
  testSExprToAst,
  testEvalAdd,
  testEvalSub,
  testEvalMul,
  testEvalDiv,
  testEvalMod,
  testsEvalAst,) where

import Ast
import SExpr
import Test.HUnit
import qualified Data.Map as Map

testSExprToAst :: Test
testSExprToAst =
  TestList
    [ "sexprToAst should handle define expression"
      ~: TestCase $ do
        let result = sexprToAst (SList [SSym "define", SSym "x", SInt 5])
        assertEqual "sexprToAst define expression" result (Right (Define "x" (Value (SInt 5))))

    , "sexprToAst should handle function call expression"
      ~: TestCase $ do
        let result = sexprToAst (SList [SSym "+", SInt 2, SInt 3])
        assertEqual "sexprToAst function call expression" result (Right (Call "+" [Value (SInt 2), Value (SInt 3)]))
    ]

testEvalAdd :: Test
testEvalAdd =
  TestList
    [ "evalAdd work"
      ~: TestCase $ do
        let result = evalAdd (Value (SInt 4)) (Value (SInt 2)) Map.empty
        assertEqual "evalAdd result" result (Right (Value (SInt 6), Map.empty))
    ]

testEvalSub :: Test
testEvalSub =
  TestList
    [ "evalSub work" ~: TestCase $ do
  let result = evalSub (Value (SInt 4)) (Value (SInt 2)) Map.empty
  assertEqual "evalSub result" result (Right (Value (SInt 2), Map.empty))
    ]

testEvalMul :: Test
testEvalMul =
  TestList
    [ "evalMul work" ~: TestCase $ do
  let result = evalMul (Value (SInt 4)) (Value (SInt 2)) Map.empty
  assertEqual "evalMul result" result (Right (Value (SInt 8), Map.empty))

    ]

testEvalDiv :: Test
testEvalDiv =
  TestList
    [ "evalDiv work"
      ~: TestCase $ do
        let result = evalDiv (Value (SInt 20)) (Value (SInt 2)) Map.empty
        assertEqual "evalDiv result" result (Right (Value (SInt 10), Map.empty))

    , "evalDiv division by 0"
      ~: TestCase $ do
        let result = evalDiv (Value (SInt 5)) (Value (SInt 0)) Map.empty
        assertEqual "evalDiv division by 0 result" result (Left "Error division by zero")
    ]

testEvalMod :: Test
testEvalMod =
  TestList
  [ "evalMod work"
      ~: TestCase $ do
        let result = evalMod (Value (SInt 21)) (Value (SInt 2)) Map.empty
        assertEqual "evalMod result" result (Right (Value (SInt 1), Map.empty))

    , "evalMod division by 0"
      ~: TestCase $ do
        let result = evalMod (Value (SInt 5)) (Value (SInt 0)) Map.empty
        assertEqual "evalMod division by 0 result" result (Left "Error modulo by zero")
  ]


-- | Tests for the 'evalAst' function.
testsEvalAst :: Test
testsEvalAst =
  TestList
    [ "evalAst 1" ~: evalAst (Value (SInt 42)) Map.empty ~?= Right (Value (SInt 42), Map.empty)
    , "evalAst 2" ~: evalAst (Call "+" [Value (SInt 2), Value (SInt 3)]) Map.empty ~?=
      Right (Value (SInt 5), Map.empty)
    , "evalAst 3" ~: evalAst (Call "-" [Value (SInt 5), Value (SInt 2)]) Map.empty ~?=
      Right (Value (SInt 3), Map.empty)
    , "evalAst 4" ~: evalAst (Call "*" [Value (SInt 2), Value (SInt 3)]) Map.empty
    ~?= Right (Value (SInt 6), Map.empty)
    , "evalAst 5" ~: evalAst (Call "div" [Value (SInt 6), Value (SInt 3)]) Map.empty
    ~?= Right (Value (SInt 2), Map.empty)
    , "evalAst 6" ~: evalAst (Call "div" [Value (SInt 6), Value (SInt 0)]) Map.empty
    ~?= Left "Error division by zero"
    , "evalAst 7" ~: evalAst (Call "mod" [Value (SInt 21), Value (SInt 2)]) Map.empty
    ~?= Right (Value (SInt 1), Map.empty)
    , "evalAst 8" ~: evalAst (Call "mod" [Value (SInt 6), Value (SInt 0)]) Map.empty
    ~?= Left "Error modulo by zero"
    , "evalAst 9" ~: evalAst (Call ">" [Value (SInt 6), Value (SInt 3)]) Map.empty
    ~?= Right (Value (SBool True), Map.empty)
    , "evalAst 10" ~: evalAst (Call ">" [Value (SInt 2), Value (SInt 6)]) Map.empty
    ~?= Right (Value (SBool False), Map.empty)
    , "evalAst 11" ~: evalAst (Call "<" [Value (SInt 6), Value (SInt 3)]) Map.empty
    ~?= Right (Value (SBool False), Map.empty)
    , "evalAst 12" ~: evalAst (Call "<" [Value (SInt 2), Value (SInt 6)]) Map.empty
    ~?= Right (Value (SBool True), Map.empty)
    , "evalAst 13" ~: evalAst (Call "eq?" [Value (SInt 6), Value (SInt 6)]) Map.empty
    ~?= Right (Value (SBool True), Map.empty)
    , "evalAst 14" ~: evalAst (Call "eq?" [Value (SInt 2), Value (SInt 6)]) Map.empty
    ~?= Right (Value (SBool False), Map.empty)
    , "evalAst 15" ~:
      evalAst (Call "if" [Value (SBool True), Value (SInt 2), Value (SInt 3)]) Map.empty
    ~?= Right (Value (SInt 2), Map.empty)
    , "evalAst 16" ~:
      evalAst (Call "if" [Value (SBool False), Value (SInt 2), Value (SInt 3)]) Map.empty
    ~?= Right (Value (SInt 3), Map.empty)
    ]
