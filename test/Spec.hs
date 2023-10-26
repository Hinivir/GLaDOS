{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Spec
-}

import Test.HUnit

import TestSExpr (
  testSExpr,
  testParserToSExprInt,
  testParserToSExprString,
  testParserToSExprList,
  testParserToSExpr
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
  testParseList,
  testStringToParser
  )

import TestVm (
  testCallOp,
  testExec
  )

main :: IO ()
main = do
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
  putStrLn "Run test for stringToParser"
  _ <- runTestTT testStringToParser
  -- testSEpr
  putStrLn "Run test for SExpr"
  _ <- runTestTT testSExpr
  putStrLn "Run test for Parser to SExpr Int"
  _ <- runTestTT testParserToSExprInt
  putStrLn "Run test for Parser to SExpr String"
  _ <- runTestTT testParserToSExprString
  putStrLn "Run test for Parser to SExpr List"
  _ <- runTestTT testParserToSExprList
  putStrLn "Run test for Parser to SExpr"
  _ <- runTestTT testParserToSExpr
  putStrLn "Run test for the Vm callOp"
  _ <- runTestTT testCallOp
  putStrLn "Run test for the Vm exec "
  _ <- runTestTT testExec
  return ()
