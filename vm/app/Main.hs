{-
-- EPITECH PROJECT, 2023
-- glados_mirror
-- File description:
-- Main.hs
-}

module Main
  ( main
  ) where

import System.Exit ()
import System.IO ()
import System.Environment

import Vm (
    Instructions,
    Env,
    exec,
    Value(..),
    )

printHelp :: IO ()
printHelp = putStrLn "USAGE: ./Vm < file.lop\nDESCRIPTION:\n\t"
    >> putStrLn "file.lop\tfile with the instruction"
    >> putStrLn "code give by glados\n\t-h\t"
    >> putStrLn "\tprint this help and exit"

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-h"] -> printHelp
        _ -> do
            fileContents <- readFile "lip.lop"
            processFileContents fileContents

processFileContents :: String -> IO ()
processFileContents fileContents =
    case lines (fileContents) of
        (instructionsLine:envLine:_) ->
            let instructions = read instructionsLine :: Instructions
                env = read envLine :: Env
            in executeInstructions env instructions
        _ -> putStrLn "Error: The file does not contain enough lines"

printValue :: Value -> IO ()
printValue (Number n) = print n
printValue (Float f) = print f
printValue (Boolean b) = print b
printValue (Char c) = print c
printValue (Op op) = print op
printValue (Builtin builtin) = print builtin
printValue (List list) = print list
printValue (String string) = print string
printValue _ = putStrLn "Error: Cannot print this value"

executeInstructions :: Env -> Instructions -> IO ()
executeInstructions env instructions = case exec [] env instructions [] of
    Right val -> printValue val
    Left err -> putStrLn $ "Error: " ++ err
