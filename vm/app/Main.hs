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

import Vm (Instructions, Env, exec)

main :: IO ()
main = do
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

executeInstructions :: Env -> Instructions -> IO ()
executeInstructions env instructions = case exec [] env instructions [] of
    Right val -> print val
    Left err -> putStrLn $ "Error: " ++ err
