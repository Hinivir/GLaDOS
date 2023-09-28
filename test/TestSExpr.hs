module TestSExpr (testSExpr, printTreeTests) where

import SExpr
import Test.HUnit

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
