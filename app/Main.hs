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

mainCheckEOF :: Bool -> IO ()
mainCheckEOF True = errorExit "No input"
mainCheckEOF False = do
                line <- getLine
                putStrLn line
                isEOF' <- isEOF
                mainCheckEOF isEOF'

main :: IO ()
main = isEOF >>= mainCheckEOF
