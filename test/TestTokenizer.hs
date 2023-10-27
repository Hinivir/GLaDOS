{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- TestTokenizer.hs
-}

module TestTokenizer
( testTokenizerCoordonate,
    testExpressTokenizedFunc,
    testExpressTokenizedTree
)
where

import Test.HUnit

import ParserStatus

import Parsing.LDataTree (
    LData(..)
    )

import Parsing.Tokenizer (
    TokenizedAny(..),
    getTokenizerCoordinates
    )

import Parsing.LDataTree.TreeToLData (
    expressTokenizedTree,
    expressTokenizedFunc
    )

testTokenizerCoordonate :: Test
testTokenizerCoordonate = TestList
    [ "Test TokenizedChar" ~: getTokenizerCoordinates (TokenizedChar 'a' (1, 2)) ~?= (1, 2)
    , "Test TokenizedInt" ~: getTokenizerCoordinates (TokenizedInt 42 (3, 4)) ~?= (3, 4)
    , "Test TokenizedFloat" ~: getTokenizerCoordinates (TokenizedFloat 3.14 (5, 6)) ~?= (5, 6)
    , "Test TokenizedString" ~: getTokenizerCoordinates (TokenizedString "Hello" (7, 8)) ~?= (7, 8)
    , "Test TokenizedLiteral" ~: getTokenizerCoordinates (TokenizedLiteral "Example" (9, 10)) ~?= (9, 10)
    , "Test TokenizedList" ~: getTokenizerCoordinates (TokenizedList 'L' [TokenizedChar 'a' (11, 12), TokenizedInt 123 (13, 14)] (15, 16)) ~?= (15, 16)
    , "Test TokenizedLine" ~: getTokenizerCoordinates (TokenizedLine [TokenizedChar 'a' (17, 18), TokenizedInt 456 (19, 20)]) ~?= (0, 0)
    , "Test TokenizedUndefined" ~: getTokenizerCoordinates TokenizedUndefined ~?= (0, 0)
    ]

testExpressTokenizedFunc :: Test
testExpressTokenizedFunc = TestList
    [ "Test TokenizedString 'true'" ~: expressTokenizedFunc [TokenizedString "true" (1, 2)] ~?= (LDataBool True (1, 2), [], createParserStatusOk)
    , "Test TokenizedString 'false'" ~: expressTokenizedFunc [TokenizedString "false" (1, 2)] ~?= (LDataBool False (1, 2), [], createParserStatusOk)
    , "Test TokenizedInt" ~: expressTokenizedFunc [TokenizedInt 42 (1, 2)] ~?= (LDataInt 42 (1, 2), [], createParserStatusOk)
    , "Test TokenizedFloat" ~: expressTokenizedFunc [TokenizedFloat 3.14 (1, 2)] ~?= (LDataFloat 3.14 (1, 2), [], createParserStatusOk)
    , "Test TokenizedLiteral" ~: expressTokenizedFunc [TokenizedLiteral "Hello" (1, 2)] ~?= (LDataString "Hello" (1, 2), [], createParserStatusOk)
    , "Test TokenizedLine" ~: expressTokenizedFunc [TokenizedLine [TokenizedInt 1 (1, 2), TokenizedString "Hello" (3, 4)]] ~?= (LDataGroup [LDataInt 1 (1, 2), LDataSymbol "Hello" (3, 4)] (0, 0), [], createParserStatusOk)
    , "Test TokenizedList '('" ~: expressTokenizedFunc [TokenizedList '(' [TokenizedInt 1 (1, 2), TokenizedInt 2 (3, 4)] (5, 6)] ~?= (LDataGroup [LDataInt 1 (1, 2), LDataInt 2 (3, 4)] (5, 6), [], createParserStatusOk)
    , "Test TokenizedList '['" ~: expressTokenizedFunc [TokenizedList '[' [TokenizedInt 1 (1, 2), TokenizedInt 2 (3, 4)] (5, 6)] ~?= (LDataUndefined,[],ParserStatusError "Unsupported group separator" "(expressList)" 3 4)
    , "Test Unsupported TokenizedList" ~: expressTokenizedFunc [TokenizedList '&' [] (1, 2)] ~?= (LDataUndefined, [], createParserStatusError "Unsupported TokenizedList starting with '&'" "(expressTokenizedFunc)" 1 2)
    , "Test TokenizedChar ':'" ~: expressTokenizedFunc [TokenizedChar ':' (1, 2)] ~?= (LDataSymbol ":" (1, 2), [], createParserStatusOk)
    , "Test Unsupported TokenizedChar" ~: expressTokenizedFunc [TokenizedChar '@' (1, 2)] ~?= (LDataUndefined, [], createParserStatusError "Unsupported TokenizedChar '@'" "(expressTokenizedFunc)" 1 2)
    , "Test Unsupported TokenizedAny" ~: expressTokenizedFunc [TokenizedUndefined] ~?= (LDataUndefined, [], createParserStatusError "Unsupported TokenizedAny" "(expressTokenizedFunc)" 0 0)
    ]

testExpressTokenizedTree :: Test
testExpressTokenizedTree = TestList
    [ "Test Empty Input" ~: expressTokenizedTree [] ~?= (Just [], createParserStatusOk)
    , "Test Single TokenizedString" ~: expressTokenizedTree [TokenizedString "Hello" (1, 2)] ~?= (Just [LDataSymbol "Hello" (1, 2)], createParserStatusOk)
    , "Test Multiple Tokens" ~: expressTokenizedTree [TokenizedString "Hello" (1, 2), TokenizedChar ':' (3, 4), TokenizedInt 42 (5, 6)] ~?= (Just [LDataSymbol "Hello" (1, 2), LDataSymbol ":" (3, 4), LDataInt 42 (5, 6)], createParserStatusOk)
    , "Test Error in Input" ~: expressTokenizedTree [TokenizedString "Hello" (1, 2), TokenizedUndefined] ~?= (Nothing, createParserStatusError "Unsupported TokenizedAny" "(expressTokenizedFunc)" 0 0)
    , "Test Nested TokenizedList" ~: expressTokenizedTree [TokenizedList '[' [TokenizedInt 1 (1, 2), TokenizedInt 2 (3, 4)] (5, 6)] ~?= (Nothing,ParserStatusError "Unsupported group separator" "(expressList)" 3 4)
    ]
