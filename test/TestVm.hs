    {-
    -- EPITECH PROJECT, 2023
    -- GLaDOS
    -- File description:
    -- test for Vm
    -}

    module TestVm
    ( testCallOp,
    )
    where

    import Test.HUnit
    import Vm (Value(..),
            Operation(..),
            Instruction(..),
            Args,
            Stack,
            Instructions,
            Env,
            resInt,
            resBool,
            resOp,
            callOp,
            execFunc,
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

