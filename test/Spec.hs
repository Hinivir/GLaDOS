{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Spec.hs
-}

import Test.HUnit

import TestParserStatus (
  testIsParserStatusOk,
  testIsParserStatusError,
  testInterpretParserStatus
  )

import TestTokenizer (
  testTokenizerCoordonate,
  testExpressTokenizedFunc,
  testExpressTokenizedTree,
  testGetLDataCoordinates
  )

import TestVm (
  testCallOp,
  testExec
  )

import TestInstruction (
    testHandleVar,
    testGetOp,
    testGetOp2
    )

main :: IO ()
main = do
    -- testParserStatus
    putStrLn "Run test for isParserStatusOk"
    _ <- runTestTT testIsParserStatusOk
    putStrLn "Run test for isParserStatusError"
    _ <- runTestTT testIsParserStatusError
    putStrLn "Run test for interpretParserStatus"
    _ <- runTestTT testInterpretParserStatus
    -- TestTokenizer
    putStrLn "Run test for the location"
    _ <- runTestTT testTokenizerCoordonate
    putStrLn "Run test for expressTokenizedFunc"
    _ <- runTestTT testExpressTokenizedFunc
    putStrLn "Run test for expressTokenizedTree"
    _ <- runTestTT testExpressTokenizedTree
    putStrLn "Run test for getLDataCoordinates"
    _ <- runTestTT testGetLDataCoordinates
    -- testVm
    putStrLn "Run test for the Vm callOp"
    _ <- runTestTT testCallOp
    putStrLn "Run test for the Vm exec "
    _ <- runTestTT testExec
    putStrLn "Run test for the Instruction handleVar"
    _ <- runTestTT testHandleVar
    putStrLn "Run test for the Instruction getOp"
    _ <- runTestTT testGetOp
    _ <- runTestTT testGetOp2
    return ()
