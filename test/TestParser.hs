{-
-- EPITECH PROJECT, 2023
-- TestParser
-- File description:
-- TestParser
-}

module TestParser (
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
  ) where

import Test.HUnit
import Parser

testParseChar :: Test
testParseChar =
    TestList
        [ "parseChar should parse a char"
            ~: runParser (parseChar 'a') "abc"
            ~?= Just ('a', "bc"),
          "parseChar should fail if the char is not present"
            ~: runParser (parseChar 'a') "bc"
            ~?= Nothing,
          "parseChar should fail if the string is empty"
            ~: runParser (parseChar 'a') ""
            ~?= Nothing
        ]

testParseAnyChar :: Test
testParseAnyChar =
  TestList
    [ "parseAnyChar should parse a char"
        ~: runParser (parseAnyChar "abc") "abc"
        ~?= Just ('a', "bc"),
      "parseAnyChar should parse a char"
        ~: runParser (parseAnyChar "bca") "cdef"
        ~?= Just ('c', "def"),
      "parseAnyChar should fail if the char is not present"
        ~: runParser (parseAnyChar "xyz") "abcd"
        ~?= Nothing,
      "parseAnyChar should fail if the string is empty"
        ~: runParser (parseAnyChar "abc") ""
        ~?= Nothing
    ]

testParseOr :: Test
testParseOr =
  TestList
    [ "parseOr Subject test 1"
      ~: runParser (parseOr (parseChar 'a') (parseChar 'b')) "abcd"
      ~?= Just ('a', "bcd"),
      "parseOr Subject test 2"
      ~: runParser (parseOr (parseChar 'a') (parseChar 'b')) "bcda"
      ~?= Just ('b', "cda"),
      "parseOr Subject test 3"
      ~: runParser (parseOr (parseChar 'a') (parseChar 'c')) "xyz"
      ~?= Nothing
    ]

testParseAnd :: Test
testParseAnd =
  TestList
    [ "parseAnd Subject test 1"
      ~: runParser (parseAnd (parseChar 'a') (parseChar 'b')) "abcd"
      ~?= Just (('a', 'b'), "cd"),
      "parseAnd Subject test 2"
      ~: runParser (parseAnd (parseChar 'a') (parseChar 'b')) "bcda"
      ~?= Nothing,
      "parseAnd Subject test 2"
      ~: runParser (parseAnd (parseChar 'a') (parseChar 'b')) "acd"
      ~?= Nothing
    ]

testParseAndWith :: Test
testParseAndWith =
  TestList
    [ "parseAndWith Subject test 1"
      ~: runParser (parseAndWith (\x y -> [x,y]) (parseChar 'a') (parseChar 'b')) "abcd"
      ~?= Just ("ab", "cd")
    ]

testParseMany :: Test
testParseMany =
  TestList
    [ "parseMany Subject test 1"
      ~: runParser (parseMany (parseChar ' ')) "    foobar"
      ~?= Just ("    ", "foobar"),
      "parseMany Subject test 2"
      ~: runParser (parseMany (parseChar ' ')) "foobar    "
      ~?= Just ("", "foobar    ")
    ]

testParseSome :: Test
testParseSome =
  TestList
    [ "parseSome Subject test 1"
      ~: runParser (parseSome (parseAnyChar ['0'..'9'])) "42foobar"
      ~?= Just ("42", "foobar"),
      "parseSome Subject test 2"
      ~: runParser (parseSome (parseAnyChar ['0'..'9'])) "foobar42"
      ~?= Nothing
    ]

testParseUInt :: Test
testParseUInt = TestList
    [ "Parse UInt from valid input" ~:
        runParser parseUInt "123 foo"
        ~?= Just (123, " foo")
    , "Parse UInt from valid input with leading spaces" ~:
        runParser parseUInt "  456 bar"
        ~?= Nothing
    , "Fail to parse UInt from invalid input" ~:
        runParser parseUInt "foo"
        ~?= Nothing
    ]

testParseInt :: Test
testParseInt = TestList
    [ "Parse Int from valid positive input" ~:
        runParser parseInt "123 baz"
        ~?= Just (123, " baz")
    , "Parse Int from valid negative input" ~:
        runParser parseInt "-789 qux"
        ~?= Just (-789, " qux")
    , "Fail to parse Int from invalid input" ~:
        runParser parseInt "quux"
        ~?= Nothing
    ]

testParsePair :: Test
--testParsePair = TestList []
testParsePair =
  TestList
    [ "parsePair Subject test 1" ~: runParser parsePair (Just (123, " baz")) ~?=
      Just ((123, 456), "foo bar")
    ]

testParseList :: Test
testParseList = TestList []
--testParseList =
--  TestList
--    [ "parseList Subject test 1" ~: runParser parseList parseInt "(1 2 3 4 5 7 11 13 17)" ~?=
--      Just ([1, 2, 3, 4, 5, 7, 11, 13, 17], "")
--    ]
