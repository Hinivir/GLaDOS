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

testSExprToAst :: Test
testSExprToAst =
  TestList
    [ "sexprToAst should handle define expression"
        ~: sexprToAst (SList [SSym "define", SSym "x", SInt 5])
        ~?= Just (Define "x" (Value (SInt 5))),
      "sexprToAst should handle function call expression"
        ~: sexprToAst (SList [SSym "+", SInt 2, SInt 3])
        ~?= Just (Call "+" [Value (SInt 2), Value (SInt 3)]),
      "sexprToAst should return Nothing for invalid input"
        ~: sexprToAst (SList [SInt 1, SInt 2, SInt 3])
        ~?= Nothing,
      "sexprToAst should handle nested expressions"
        ~: sexprToAst (SList [SSym "define", SSym "x", SList [SSym "+", SInt 2, SInt 3]])
        ~?= Just (Define "x" (Call "+" [Value (SInt 2), Value (SInt 3)]))
    ]

testEvalAdd :: Test
testEvalAdd =
  TestList
    [ "evalAdd work"
      ~: evalAdd (Value (SInt 4)) (Value (SInt 2))
      ~?= Just (Value (SInt 6))
    ]

testEvalSub :: Test
testEvalSub =
  TestList
    [ "evalSub work" ~: evalSub (Value (SInt 4)) (Value (SInt 2)) ~?=
      Just (Value (SInt 2))
    ]

testEvalMul :: Test
testEvalMul =
  TestList
    [ "evalMul work" ~: evalMul (Value (SInt 4)) (Value (SInt 2)) ~?=
      Just (Value (SInt 8))
    ]

testEvalDiv :: Test
testEvalDiv =
  TestList
    [ "evalDiv work" ~: evalDiv (Value (SInt 20)) (Value (SInt 2)) ~?=
      Just (Value (SInt 10)),
      "evalDiv division by 0" ~: evalDiv (Value (SInt 5)) (Value (SInt 0))
      ~?= Nothing
    ]

testEvalMod :: Test
testEvalMod =
  TestList
  [ "evalDiv work" ~: evalMod (Value (SInt 21)) (Value (SInt 2)) ~?=
    Just (Value (SInt 1))
  , "evalDiv division by 0" ~: evalMod (Value (SInt 5)) (Value (SInt 0)) ~?=
    Nothing
  ]


-- | Tests for the 'evalAst' function.
testsEvalAst :: Test
testsEvalAst =
  TestList
    [ "evalAst 1" ~: evalAst (Value (SInt 42)) ~?= Just (Value (SInt 42)),
      "evalAst 2" ~: evalAst (Call "+" [Value (SInt 2), Value (SInt 3)]) ~?= Just (Value (SInt 5)),
      "evalAst 3" ~: evalAst (Call "-" [Value (SInt 5), Value (SInt 2)]) ~?= Just (Value (SInt 3)),
      "evalAst 4" ~: evalAst (Call "*" [Value (SInt 2), Value (SInt 3)]) ~?= Just (Value (SInt 6)),
      "evalAst 5" ~: evalAst (Call "div" [Value (SInt 6), Value (SInt 3)]) ~?= Just (Value (SInt 2)),
      "evalAst 6" ~: evalAst (Call "div" [Value (SInt 6), Value (SInt 0)]) ~?= Nothing,
      "evalAst 7" ~: evalAst (Call "mod" [Value (SInt 21), Value (SInt 2)]) ~?= Just (Value (SInt 1)),
      "evalAst 8" ~: evalAst (Call "mod" [Value (SInt 6), Value (SInt 0)]) ~?= Nothing,
      "evalAst 9" ~: evalAst (Call ">" [Value (SInt 6), Value (SInt 3)]) ~?= Just (Value (SBool True)),
      "evalAst 10" ~: evalAst (Call ">" [Value (SInt 2), Value (SInt 6)]) ~?= Just (Value (SBool False)),
      "evalAst 11" ~: evalAst (Call "<" [Value (SInt 6), Value (SInt 3)]) ~?= Just (Value (SBool False)),
      "evalAst 12" ~: evalAst (Call "<" [Value (SInt 2), Value (SInt 6)]) ~?= Just (Value (SBool True)),
      "evalAst 13" ~: evalAst (Call "eq?" [Value (SInt 6), Value (SInt 6)]) ~?= Just (Value (SBool True)),
      "evalAst 14" ~: evalAst (Call "eq?" [Value (SInt 2), Value (SInt 6)]) ~?= Just (Value (SBool False)),
      "evalAst 15" ~: evalAst (Call "if" [Value (SBool True), Value (SInt 2), Value (SInt 3)]) ~?= Just (Value (SInt 2)),
      "evalAst 16" ~: evalAst (Call "if" [Value (SBool False), Value (SInt 2), Value (SInt 3)]) ~?= Just (Value (SInt 3))
    ]
