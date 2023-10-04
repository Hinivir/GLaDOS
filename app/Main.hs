{- 
-- EPITECH PROJECT, 2023
-- glados_mirror
-- File description:
-- Main.hs
-}

module Main (main) where

import System.IO
import System.Exit

-- function errorExit
-- Take a String
-- Prints it on error output with exitWith 84 (for indicates an error)
errorExit :: String -> IO ()
errorExit msg = hPutStrLn stderr msg >> exitWith (ExitFailure 84)

-- function mainCheckEOF
-- Take a Boolean
-- If true: then calls errorExit with "No input"
-- If false: read standard input, display it, continue to the end of the file
readLines :: IO [String]
readLines = do
    line <- getLine
    isEOF' <- isEOF
    if isEOF'
        then return [line]
        else do
            rest <- readLines
            return (line : rest)

main :: IO ()
main = do
    isEOF' <- isEOF
    if isEOF'
        then errorExit "No input"
        else do
            linesList <- readLines
            mapM_ putStrLn linesList

