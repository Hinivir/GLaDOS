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
mainCheckEOF :: IO ()
mainCheckEOF = do
    line <- getLine
    putStrLn line
    isEOF' <- isEOF
    case isEOF' of
        True -> return ()
        False -> mainCheckEOF

-- function main
-- Take IO (it's the main)
-- If no standard input then error
-- Otherwise passes standard input to the rest of the program
main :: IO ()
main = do
    isEOF' <- isEOF
    case isEOF' of
        True -> errorExit "No input"
        False -> mainCheckEOF
