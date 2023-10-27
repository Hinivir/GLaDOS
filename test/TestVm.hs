{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- test for Vm
-}

module TestVm
( testCallOp,
    testExec
)
where

import Test.HUnit

import Vm (Value(..),
        Operation(..),
        Instruction(..),
        Builtin(..),
        callOp,
        exec)

testCallOp :: Test
testCallOp =
   TestList
        [ "Addition" ~: callOp Add (Number 2) (Number 3) @?= Right (Number 5),
          "Division" ~: callOp Div (Number 4) (Number 2) @?= Right (Number 2),
          "Subtraction" ~: callOp Sub (Number 5) (Number 3) @?= Right (Number 2),
          "Multiplication" ~: callOp Mul (Number 5) (Number 3) @?= Right (Number 15),
          "Modulo" ~: callOp Mod (Number 10) (Number 3) @?= Right (Number 1),
          "Modulo by zero" ~: callOp Mod (Number 4) (Number 0) @?= Left "Error: modulo by zero",
          "Division by zero" ~: callOp Div (Number 4) (Number 0) @?= Left "Error: division by zero",
          "Test Equal Number" ~: callOp Eq (Number 5) (Number 5) ~?= Right (Boolean True),
          "Test Not Equal Number" ~: callOp Eq (Number 5) (Number 7) @?= Right (Boolean False),
          "Test Equal Boolean" ~: callOp Eq (Boolean True) (Boolean True) @?= Right (Boolean True),
          "Test Not Equal Boolean" ~: callOp Eq (Boolean True) (Boolean False) @?= Right (Boolean False),
          "Test Less Number" ~: callOp Less (Number 3) (Number 7) @?= Right (Boolean True),
          "Test Not Less Number" ~: callOp Less (Number 7) (Number 3) @?= Right (Boolean False),
          "Test Greater Number" ~: callOp Greater (Number 7) (Number 3) @?= Right (Boolean True),
          "Test Not Greater Number" ~: callOp Greater (Number 3) (Number 7) @?= Right (Boolean False)
        ]

testExec :: Test
testExec =
    TestList
        [ "Test exec with valid input" ~:
            exec [Number 1] [] [PushArg 0, Push (Number 1), Push (Op Add), Call, Ret] [] ~?= Right (Number 2)
        , "Test exec with invalid input" ~:
            exec [Boolean True] [] [PushArg 0, Push (Number 1), Push (Op Add), Call, Ret] [] ~?= Left "Error: invalid operation"
        , "Test exec with division" ~:
            exec [Number 2, Number 6] [] [PushArg 0, PushArg 1, Push (Op Div), Call, Ret] [] ~?= Right (Number 3)
        , "Test exec des lists" ~:
            exec [Number 1] [] [Push (List [(Number 1), (Number 2), (Number 3)]), Push (Builtin Tail), Call, Ret] [] ~?= Right (List [(Number 2), (Number 3)])
        , "Test exec with invalid operation" ~:
            exec [Boolean True] [] [PushArg 0, Push (Number 1), Push (Op Add), Call, Ret] [] ~?= Left "Error: invalid operation"
        , "Test exec with empty stack" ~:
            exec [] [] [Push (Number 1), Push (Op Add), Call, Ret] [] ~?= Left "Invalid instruction"
        , "Test JumpIfFalse with true condition" ~:
            exec [] [] [Push (Boolean True), JumpIfFalse 3, Push (Number 1), Call, Ret] [] ~?= Left "Invalid instruction"
        , "Test JumpIfFalse with false condition" ~:
            exec [] [] [Push (Boolean False), JumpIfFalse 3, Push (Number 1), Call, Ret] [] ~?= Left "Invalid instruction"
        , "Test Jump with valid jump" ~:
            exec [] [] [Push (Number 1), Jump 3, Push (Number 2), Call, Ret] [] ~?= Left "Invalid instruction"
        , "Test Jump with invalid jump" ~:
            exec [] [] [Push (Number 1), Jump 3, Push (Number 2), Call, Ret] [] ~?= Left "Invalid instruction"
        ]
