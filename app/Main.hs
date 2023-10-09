{-
-- EPITECH PROJECT, 2023
-- glados_mirror
-- File description:
-- Main.hs
-}

module Main (main) where

import System.IO
import System.Exit
import Parser(stringToParser, ParserAny)
import ParserToSExpr(parserToSExpr)

-- function errorExit
-- Take a String
-- Prints it on error output with exitWith 84 (for indicates an error)
errorExit :: String -> IO ()
errorExit msg = hPutStrLn stderr msg >> exitWith (ExitFailure 84)

-- function mainCheckEOF
-- Take a Boolean
-- If true: then calls errorExit with "No input"
-- If false: read standard input, display it, continue to the end of the file

readLines :: IO String
readLines = do
    line <- getLine
    isEOF' <- isEOF
    if isEOF'
        then return ""
        else do
            rest <- readLines
            return (line ++ "\n" ++ rest)


getSExpr :: Maybe [ParserAny] -> IO()
getSExpr Nothing = errorExit "Parse error"
getSExpr a = print (parserToSExpr a)


parseLineList :: String -> IO()
parseLineList _ = errorExit "Parse error"
--parseLineList linesList = case stringToParser linesList of
--        Nothing -> errorExit "Parse error"
--        parsedLines' -> getSExpr parsedLines'


main :: IO ()
main = do
    isEOF' <- isEOF
    if isEOF'
        then errorExit "No input"
        else do
            linesList <- readLines
            parseLineList linesList

--linesList  >  StringToParser   >  [Just(ParseAny)]  >  ParserToSExpr  >  Just SExpr  >  SExprToAst  >  Just Ast  >  Eval
--                               >  Nothing                             >  Nothing                    >  Nothing
--    x                 x               x                   x                  x
