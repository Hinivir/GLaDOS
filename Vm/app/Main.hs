{-
-- EPITECH PROJECT, 2023
-- glados_mirror
-- File description:
-- Main.hs
-}

module Main
  ( main
  ) where

import System.Exit
import System.IO

import Vm (Instructions, Env, exec)

main :: IO ()
main = do
    fileContents <- readFile "lip.lop"
    let (instructionsLine:envLine:_) = lines fileContents
    let instructions = read instructionsLine :: Instructions
    let env = read envLine :: Env
    let result = exec [] env instructions []

    case result of
        Right val -> print val
        Left err -> putStrLn $ "Error: " ++ err
