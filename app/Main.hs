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
import System.Environment

import Parsing (parsingToInstruct)
import Vm (Env, Instructions)
import ParserStatus (
  ParserStatus(..), interpretParserStatus, isParserStatusError)

-- | Function that take a String and print it
-- | Print the String and exit with an error
errorExit :: String -> IO ()
errorExit msg = hPutStrLn stderr msg >> exitWith (ExitFailure 84)

-- | Function that read lines until the end of the file
-- | Return a table of String with all the lines
readLines :: IO [String]
readLines = do
  line <- getLine
  isEOF' <- isEOF
  if isEOF'
    then return [line]
    else do
      rest <- readLines
      return (line : rest)

createFile :: (Maybe Instructions, Env, ParserStatus) -> IO ()
createFile (Nothing, _, status)
  | isParserStatusError status = errorExit $ interpretParserStatus status
  | otherwise                  = errorExit "No input"
createFile (Just instruct, env, _) =
  withFile "lip.lop" WriteMode $ \handle ->
    hPrint handle instruct >> hPrint handle env

-- | Function to print the help message
printHelp :: IO ()
printHelp = putStrLn "USAGE: ./glados < file.lip\nDESCRIPTION:\n\t"
    >> putStrLn "file.lip\tfile with instructions\n\t-h\t"
    >> putStrLn "\tprint this help and exit"

-- | The main function
-- | Read the lines, parse them and print the result
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-h"] -> printHelp
    _      -> do
                linesTable <- readLines
                createFile (parsingToInstruct linesTable)
