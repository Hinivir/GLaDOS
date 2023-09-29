{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Main
-}

module Main (main) where

import System.IO
import System.Exit


errorExit :: String -> IO ()
errorExit msg = hPutStrLn stderr msg >> exitWith (ExitFailure 84)

mainCheckFile :: IO ()
mainCheckFile = do
    contents <- getContents
    putStr contents

main :: IO ()
main = mainCheckFile

