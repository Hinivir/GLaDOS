{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Spec
-}

import Test.HUnit
import TestSExpr (printTreeTests, testSExpr)
import TestAst (
  testSExprToAst,
  testEvalAdd,
  testEvalSub,
  testEvalMul,
  testEvalDiv,
  testEvalMod,
  testsEvalAst
  )
import TestParser (
  testParseChar,
  testParseAnyChar,
  testParseOr,
  testParseAnd,
  testParseAndWith,
  testParseMany,
  testParseSome,
  testParseUInt,
  testParseInt,
  testParsePair,
  testParseList
  )

main :: IO ()
main = do
  putStrLn "Run test for SExpr"
  _ <- runTestTT testSExpr
  putStrLn "Run test for printTree"
  _ <- runTestTT printTreeTests
  putStrLn "Run test for SExpr to Ast"
  _ <- runTestTT testSExprToAst
  putStrLn "Run test for EvalAdd"
  _ <- runTestTT testEvalAdd
  putStrLn "Run test for EvalSub"
  _ <- runTestTT testEvalSub
  putStrLn "Run test for EvalMul"
  _ <- runTestTT testEvalMul
  putStrLn "Run test for EvalDiv"
  _ <- runTestTT testEvalDiv
  putStrLn "Run test for EvalMod"
  _ <- runTestTT testEvalMod
  putStrLn "Run test for EvalAst"
  _ <- runTestTT testsEvalAst
  -- testParse
  putStrLn "Run test for parseChar"
  _ <- runTestTT testParseChar
  putStrLn "Run test for parseAnyChar"
  _ <- runTestTT testParseAnyChar
  putStrLn "Run test for parseOr"
  _ <- runTestTT testParseOr
  putStrLn "Run test for parseAnd"
  _ <- runTestTT testParseAnd
  putStrLn "Run test for parseAndWith"
  _ <- runTestTT testParseAndWith
  putStrLn "Run test for parseMany"
  _ <- runTestTT testParseMany
  putStrLn "Run test for parseSome"
  _ <- runTestTT testParseSome
  putStrLn "Run test for parseUInt"
  _ <- runTestTT testParseUInt
  putStrLn "Run test for parseInt"
  _ <- runTestTT testParseInt
  putStrLn "Run test for parsePair"
  _ <- runTestTT testParsePair
  putStrLn "Run test for parseList"
  _ <- runTestTT testParseList
  return ()
