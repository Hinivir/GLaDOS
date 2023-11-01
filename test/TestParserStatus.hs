{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- TestParserStatus.hs
-}


module TestParserStatus (
  testInterpretParserStatus,
  testIsParserStatusError,
  testIsParserStatusOk
) where

import ParserStatus
import Test.HUnit

testIsParserStatusOk :: Test
testIsParserStatusOk =
  TestList
    [
      "isParserStatusOk ParserStatusOK" ~:
        isParserStatusOk createParserStatusOk
        ~?= True,
      "isParserStatusOk ParserStatusError Generic" ~:
        isParserStatusOk createParserStatusErrorGeneric
        ~?= False,
      "isParserStatusOk ParserStatusError Simple" ~:
        isParserStatusOk (createParserStatusErrorSimple "Test Error" "Test info")
        ~?= False,
      "isParserStatusOk ParserStatusError Complete" ~:
        isParserStatusOk (createParserStatusError "Test Error" "Test info" 4 20)
        ~?= False
    ]

testIsParserStatusError :: Test
testIsParserStatusError =
  TestList
    [
      "isParserStatusError ParserStatusOK" ~:
        isParserStatusError createParserStatusOk
        ~?= False,
      "isParserStatusError ParserStatusError Generic" ~:
        isParserStatusError createParserStatusErrorGeneric
        ~?= True,
      "isParserStatusError ParserStatusError Simple" ~:
        isParserStatusError (createParserStatusErrorSimple "Test Error" "Test info")
        ~?= True,
      "isParserStatusError ParserStatusError Complete" ~:
        isParserStatusError (createParserStatusError "Test Error" "Test info" 4 20)
        ~?= True
    ]

testInterpretParserStatus :: Test
testInterpretParserStatus =
  TestList
    [
      "interpretParserStatus ParserStatusOK" ~:
        interpretParserStatus createParserStatusOk
        ~?= "OK",
      "interpretParserStatus ParserStatusError Generic" ~:
        interpretParserStatus createParserStatusErrorGeneric
        ~?= "Unknown Error",
      "interpretParserStatus ParserStatusError Simple BothFilled" ~:
        interpretParserStatus (createParserStatusErrorSimple "Test Error" "This is why it doesn't work")
        ~?= "Test Error: This is why it doesn't work",
      "interpretParserStatus ParserStatusError Simple BothEmpty" ~:
        interpretParserStatus (createParserStatusErrorSimple "" "")
        ~?= "Unknown Error",
      "interpretParserStatus ParserStatusError Simple NameEmpty" ~:
        interpretParserStatus (createParserStatusErrorSimple "" "This is why it should work")
        ~?= "Unknown Error: This is why it should work",
      "interpretParserStatus ParserStatusError Simple InfoEmpty" ~:
        interpretParserStatus (createParserStatusErrorSimple "Some Error" "")
        ~?= "Some Error",
      "interpretParserStatus ParserStatusError Complete AllFilled" ~:
        interpretParserStatus (createParserStatusError "Test Error" "This is why it doesn't work" 4 20)
        ~?= "Test Error: This is why it doesn't work (Ln 4, Col 20)",
      "interpretParserStatus ParserStatusError Complete NameInfoEmpty" ~:
        interpretParserStatus (createParserStatusError "" "" 4 20)
        ~?= "Unknown Error (Ln 4, Col 20)"
    ]
