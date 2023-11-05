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
    let linesList = lines fileContents
    case linesList of
        (instructionsLine:envLine:_) -> do
            let instructions = read instructionsLine :: Instructions
            let env = read envLine :: Env
            let result = exec [] env instructions []
            case result of
                Right val -> print val
                Left err -> putStrLn $ "Error: " ++ err
        _ -> putStrLn "Error: The file does not contain enough lines"

