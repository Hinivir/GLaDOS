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

import Parsing (parsingToInstruct)
import Vm (Env, Instructions)
import ParserStatus (ParserStatus(..))

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
createFile (Nothing, _, ParserStatusOK) = errorExit "No input"
createFile (Nothing, _, ParserStatusError _ errorMsg line col) =
  errorExit $ "Error at line " ++ show line ++ ", column "
  ++ show col ++ ": " ++ show errorMsg
createFile (Just instruct, env, _) =
  withFile "lip.lop" WriteMode $ \handle ->
    hPrint handle instruct >> hPrint handle env

-- | The main function
-- | Read the lines, parse them and print the result
main :: IO ()
main = do
  isEOF' <- isEOF
  if isEOF'
    then errorExit "No input"
    else do
      linesTable <- readLines
      createFile (parsingToInstruct linesTable)
