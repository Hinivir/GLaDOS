{-
-- EPITECH PROJECT, 2023
-- glados_mirror
-- File description:
-- Main.hs
-}

module Main
  ( main
  ) where

import Ast (sexprToAst, evalAst, Ast(..))
import Parser (ParserAny, stringToParser)
import ParserToSExpr (parserToSExpr)
import SExpr (SExpr(..))
import System.Exit
import System.IO
import qualified Data.Map as Map

-- | Function that take a String and print it
-- | Print the String and exit with an error
errorExit :: String -> IO ()
errorExit msg = hPutStrLn stderr msg >> exitWith (ExitFailure 84)

-- | Function that read lines until the end of the file
-- | Return a String with all the lines
readLines :: IO String
readLines = do
  line <- getLine
  isEOF' <- isEOF
  if isEOF'
    then return line
    else do
      rest <- readLines
      return (line ++ "\n" ++ rest)

getResult :: Ast -> IO ()
getResult (Value (SInt x)) = print x
getResult (Value (SBool True)) = putStrLn "#t"
getResult (Value (SBool False)) = putStrLn "#f"
getResult _ = error "error while getting result"

-- | Function that take a SExpr and print it
-- | Print the Ast if the SExpr is correct
-- | Print "error while getting ast" if the SExpr is incorrect
getAst :: SExpr -> IO ()
getAst sexpr =
  case sexprToAst sexpr of
    Left str -> errorExit str
    Right ast -> case evalAst ast Map.empty of
      Nothing -> errorExit "error with eval"
      Just (str, _) -> getResult str

-- | Function that take a Maybe [ParserAny] and print it
-- | Print the SExpr if the Maybe [ParserAny] is correct
-- | Print "error while getting sexpr" if the Maybe [ParserAny] is incorrect
getSExpr :: Maybe [ParserAny] -> IO ()
getSExpr a =
  case parserToSExpr a of
    Nothing    -> errorExit "error while getting sexpr"
    Just sexpr -> getAst sexpr

-- | Function that take a String and print it
-- | Print the Maybe [ParserAny] if the String is correct
-- | Print "error while getting parser" if the String is incorrect
parseLineList :: String -> IO ()
parseLineList linesList =
  case stringToParser linesList of
    Nothing      -> errorExit "error while getting parser"
    parsedLines' -> getSExpr parsedLines'

-- | Function that take a String and print it
-- | Print the Maybe [ParserAny] if the String is correct
-- | Print "error while getting parser" if the String is incorrect
main :: IO ()
main = do
  isEOF' <- isEOF
  if isEOF'
    then errorExit "No input"
    else do
      linesList <- readLines
      parseLineList linesList
