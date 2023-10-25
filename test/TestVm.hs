{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- test for Vm
-}

module TestVm (
  testCallOp,
  testExec
) where

import Vm
import Test.HUnit

testCallOp :: Test
testCallOp = TestList
    [ "Addition" ~: callOp Add (Number 2) (Number 3) @?= Right (Number 5)
    , "Division" ~: callOp Div (Number 4) (Number 2) @?= Right (Number 2)
    , "Substraction" ~: callOp Sub (Number 5) (Number 3) @?= Right (Number 2)
    , "Multiplication" ~: callOp Mul (Number 5) (Number 3) @?= Right (Number 15)
    , "Division by zero" ~: callOp Div (Number 4) (Number 0) @?= Left "Error: division by zero"
    -- , "Modulo by zerp" ~: callOp Mod (Number 4) (Number 0) @?= Left "Error: division by zero"
    ]

-- Test pour exec
testExec :: Test
testExec = TestList
    [ "Push and Call" ~: exec [] [] [Push (Number 42), Call] [] @?= Right (Number 42)

    ]
