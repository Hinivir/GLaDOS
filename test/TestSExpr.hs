{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- test for SExpr
-}

module TestSExpr (
  testSExpr,
  printTreeTests,
  testParserToSExprInt,
  testParserToSExprList,
  testParserToSExpr
) where

import SExpr
import ParserToSExpr
import Test.HUnit
import Parser (
  stringToParser,
  ParserAny(..)
  )

-- Define test cases
testSExpr :: Test
testSExpr =
  TestList
    [ "getSymbol should return Just symbol"
        ~: getSymbol (SSym "hello")
        ~?= Just "hello",
      "getSymbol should return Nothing for an integer"
        ~: getSymbol (SInt 42)
        ~?= Nothing,
      "getInteger should return Just integer"
        ~: getInteger (SInt 42)
        ~?= Just 42,
      "getInteger should return Nothing for a symbol"
        ~: getInteger (SSym "hello")
        ~?= Nothing,
      "getList should return Just list"
        ~: getList (SList [SInt 1, SInt 2, SInt 3])
        ~?= Just [SInt 1, SInt 2, SInt 3],
      "getList should return Nothing for an integer"
        ~: getList (SInt 42)
        ~?= Nothing,
      "getList should return Nothing for a symbol"
        ~: getList (SSym "hello")
        ~?= Nothing
    ]

-- Define test cases for printTree
printTreeTests :: Test
printTreeTests =
  test
    [ "printTree should handle (define x 5)"
        ~: printTree (SList [SSym "define", SSym "x", SInt 5])
        ~?= "a List with (a Symbol 'define', a Symbol 'x', a Number 5)",
      "printTree should handle x"
        ~: printTree (SSym "x")
        ~?= "a Symbol 'x'",
      "printTree should handle (define y (+ 5 x))"
        ~: printTree (SList [SSym "define", SSym "y", SList [SSym "+", SInt 5, SSym "x"]])
        ~?= "a List with (a Symbol 'define', a Symbol 'y', a List with (a Symbol '+', a Number 5, a Symbol 'x'))"
    ]

-- PARSER TO SEXPR

testParserToSExprInt :: Test
testParserToSExprInt =
  test
    [
      "parserToSExprInt Lipatant 1" ~:
        runParserToSexpr parserToSExprInt [(ParserInt 10),(ParserInt 11),(ParserInt 12)]
        ~?= Just ((SInt 10),[(ParserInt 11),(ParserInt 12)]),
      "parserToSExprInt Lipatant 2" ~:
        runParserToSexpr parserToSExprInt [(ParserInt 10)]
        ~?= Just ((SInt 10), []),
      "parserToSExprInt Lipatant 3" ~:
        runParserToSexpr parserToSExprInt [(ParserInt (-42))]
        ~?= Just ((SInt (-42)), []),
      "parserToSExprInt Lipatant 4" ~:
        runParserToSexpr parserToSExprInt [(ParserChar 'E')]
        ~?= Nothing
    ]

testParserToSExprList :: Test
testParserToSExprList =
  test
    [
      "parserToSExprList Empty Parser" ~:
        runParserToSexpr parserToSExprList []
        ~?= Nothing,
      "parserToSExprList Empty List" ~:
        runParserToSexpr parserToSExprList [(ParserChar ')')]
        ~?= Just (SList [], []),
      "parserToSExprList Lipatant 1" ~:
        runParserToSexpr parserToSExprList [(ParserInt 10),(ParserInt 11),(ParserInt 12)]
        ~?= Nothing,
      "parserToSExprList Lipatant 2" ~:
        runParserToSexpr parserToSExprList [(ParserInt 10),(ParserInt 11),(ParserChar ')'),(ParserInt 12)]
        ~?= Just (SList [(SInt 10),(SInt 11)], [(ParserInt 12)]),
      "parserToSExprList Lipatant 3" ~:
        runParserToSexpr parserToSExprList [(ParserInt 10),(ParserInt 11),(ParserChar ')'),(ParserInt 12),(ParserInt 13)]
        ~?= Just (SList [(SInt 10),(SInt 11)], [(ParserInt 12),(ParserInt 13)])
    ]

testParserToSExpr :: Test
testParserToSExpr =
  test
    [
      "parserToSExpr Lipatant 1" ~:
        parserToSExpr (stringToParser "12345")
        ~?= Nothing
--      "parserToSExpr Lipatant 2" ~:
--        parserToSExpr (stringToParser "(define x 5)")
--        ~?= Nothing,
--      "parserToSExpr Lipatant 3" ~:
--        parserToSExpr (stringToParser "(define x 5)\nx(if (> x 4) 1 0)\n(define y (+ 5 x))")
--        ~?= Nothing
    ]
