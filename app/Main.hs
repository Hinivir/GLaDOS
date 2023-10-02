module Main (main) where

import System.IO
import System.Exit

-- Print an error message to stderr and exit with code 84
errorExit :: String -> IO ()
errorExit msg = hPutStrLn stderr msg >> exitWith (ExitFailure 84)

-- Check for EOF and process standard input
mainCheckEOF :: IO ()
mainCheckEOF = do
    line <- getLine
    putStrLn line
    isEOF' <- isEOF
    if isEOF' then return () else mainCheckEOF

main :: IO ()
main = do
    isEOF' <- isEOF
    if isEOF' then errorExit "No input" else mainCheckEOF
