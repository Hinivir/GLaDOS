{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- test ast
-}


module TestAst (testSExprToAst, testsEvalAst) where

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

-- | Tests for the 'evalAst' function.
testsEvalAst :: Test
testsEvalAst =
  TestList
    [ "evalAst 1" ~: evalAst (Value (SInt 42)) ~?= Just (Value (SInt 42)),
      "evalAst 2" ~: evalAst (Call "+" [Value (SInt 2), Value (SInt 3)]) ~?= Just (Value (SInt 5)),
      "evalAst 3" ~: evalAst (Call "-" [Value (SInt 5), Value (SInt 2)]) ~?= Just (Value (SInt 3)),
      "evalAst 4" ~: evalAst (Call "*" [Value (SInt 2), Value (SInt 3)]) ~?= Just (Value (SInt 6)),
      "evalAst 5" ~: evalAst (Call "/" [Value (SInt 6), Value (SInt 3)]) ~?= Just (Value (SInt 2)),
      "evalAst 6" ~: evalAst (Call "/" [Value (SInt 6), Value (SInt 0)]) ~?= Nothing
    ]
