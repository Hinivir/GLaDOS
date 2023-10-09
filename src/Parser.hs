{-
-- EPITECH PROJECT, 2023
-- Parser
-- File description:
-- Parser
-}
{-|
Module      : Parser
Description : Définition de types pour les analyseurs syntaxiques (parsers)

Ce module définit trois types : `Parser`, `Error` et `BetterParser`.

Le type `Parser` est une fonction qui prend une chaîne de caractères en entrée et renvoie une paire contenant une valeur de type `a` et la chaîne de caractères restante. Si la chaîne d'entrée ne peut pas être analysée, la fonction renvoie `Nothing`.

Le type `Error` est simplement une chaîne de caractères qui représente une erreur.

Le type `BetterParser` est similaire à `Parser`, mais renvoie également une chaîne d'erreur en cas d'échec d'analyse.
-}
module Parser
  ( Parser
  , ParserAny (ParserChar, ParserInt, ParserString)
  , runParser
  , BetterParser
  , parseChar
  , parseAnyChar
  , parseOr
  , parseAnd
  , parseAndWith
  , parseMany
  , parseSome
  , parseUInt
  , parseInt
  , parsePair
  , parseList
  , stringToParser
  ) where

import Data.Char (isDigit)

--type Parser a = String -> Maybe (a, String)

type Error = Maybe String

type BetterParser a = String -> Maybe (a, Error, String)

-- | Parse a char in a string
--
-- Returns 'Nothing' if the char is not in the string.
-- Returns 'Just' the char and the rest of the string.
--parseChar :: Char -> Parser Char
--parseChar c (x:xs)
--  | x == c = Just (c, xs)
--parseChar _ _ = Nothing*

data Parser a =
  Parser
    { runParser :: String -> Maybe (a, String)
    }

data ParserAny = ParserChar Char | ParserInt Int | ParserString String
  deriving (Eq, Show)

-- | Parse a char in a string
--
-- Returns 'Nothing' if the char is not in the string.
-- Returns 'Just' the char and the rest of the string.
parseChar :: Char -> Parser Char
parseChar c =
  Parser $ \input ->
    case input of
      (x:xs)
        | x == c -> Just (c, xs)
      _ -> Nothing

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 =
  Parser $ \input ->
    case runParser p1 input of
      Just (result, remaining) -> Just (result, remaining)
      Nothing -> runParser p2 input

-- | Parse a list of char in a string
--
-- Returns 'Nothing' if ther is not the char in the string.
-- Returns 'Just' the char and the rest of the string.
parseAnyChar :: String -> Parser Char
parseAnyChar (x:xs) = parseOr (parseChar x) (parseAnyChar xs)
parseAnyChar []     = Parser $ const Nothing


parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 = parseAndWith (,) p1 p2

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f p1 p2 =
  Parser $ \input ->
    case runParser p1 input of
      Just (result1, remaining1) ->
        case runParser p2 remaining1 of
          Just (result2, remaining2) -> Just (f result1 result2, remaining2)
          Nothing                    -> Nothing
      Nothing -> Nothing


parseMany :: Parser a -> Parser [a]
parseMany p =
  Parser $ \input ->
    case runParser p input of
      Just (result, remaining) ->
        case runParser (parseMany p) remaining of
          Just (results, remaining') -> Just (result : results, remaining')
          Nothing                    -> Just ([result], remaining)
      Nothing -> Just ([], input)


parseSome :: Parser a -> Parser [a]
parseSome p =
  Parser $ \input ->
    case runParser p input of
      Just (result, remaining) ->
        case runParser (parseMany p) remaining of
          Just (results, remaining') -> Just (result : results, remaining')
          Nothing                    -> Just ([result], remaining)
      Nothing -> Nothing


parseUInt :: Parser Int
parseUInt =
  Parser $ \input ->
    case span isDigit input of
      ("", _)             -> Nothing -- No digits found, parsing failed.
      (digits, remaining) -> Just (read digits, remaining)

parseIntCheckNegative :: Parser Int
parseIntCheckNegative =
      Parser $ \input ->
        case input of
          ('-':rest) ->
            case runParser parseUInt rest of
              Just (unsigned, remaining) -> Just (-unsigned, remaining)
              Nothing                    -> Nothing
          _ -> Nothing

parseInt :: Parser Int
parseInt =
  Parser $ \input ->
    case runParser parseNegativeInt input of
      Nothing -> runParser parseUInt input
      result -> result
  where
    parseNegativeInt = parseIntCheckNegative

-- Parse a pair of integers enclosed in parentheses
parsePair :: Parser a -> Parser (a, a)
parsePair parser =
  Parser $ \input1 -> do
    (_, input2) <- runParser (parseChar '(') input1
    (y, input3) <- runParser parser input2
    (_, input4) <- runParser (parseChar ' ') input3
    (z, input5) <- runParser parser input4
    (_, input6) <- runParser (parseChar ')') input5
    return ((y, z), input6)

parseListSegment :: Parser a -> [a] -> Parser [a]
parseListSegment parser list =
  Parser $ \input1 -> do
    (element, input2) <- runParser parser input1
    case runParser (parseChar ' ') input2 of
      Just (_, b) -> runParser (runParseListSegment (list ++ [element])) b
      Nothing     -> Just ((list ++ [element]), input2)
    where
      runParseListSegment = parseListSegment parser

parseList :: Parser a -> Parser [a]
parseList parser =
  Parser $ \input1 -> do
    (_, input2) <- runParser (parseChar '(') input1
    (list, input3) <- runParser (parseListSegment parser []) input2
    case runParser (parseChar ')') input3 of
      Just (_, input4)  -> Just (list, input4)
      Nothing           -> Nothing

-- STRING TO PARSER --

stringToParserAdd :: ParserAny -> String -> Maybe [ParserAny]
stringToParserAdd element string = case stringToParser string of
  Just list -> Just (element:list)
  Nothing   -> Nothing

stringToParserIsEmpty :: Char -> Bool
stringToParserIsEmpty ' ' = True
stringToParserIsEmpty '\n' = True
stringToParserIsEmpty _ = False

stringToParserIsUnique :: Char -> Bool
stringToParserIsUnique '(' = True
stringToParserIsUnique ')' = True
stringToParserIsUnique _ = False

stringListOperators :: [Char]
stringListOperators = "+-*/%<>=" ++ "?!"

stringList :: [Char]
stringList = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ stringListOperators

stringToParserCaseUnique :: String -> Maybe [ParserAny]
stringToParserCaseUnique []    = Nothing
stringToParserCaseUnique (h:t) = case runParser (parseChar h) (h:t) of
  Just (char, inp)  -> stringToParserAdd (ParserChar char) inp
  Nothing           -> Nothing

stringToParserCaseDigit :: String -> Maybe [ParserAny]
stringToParserCaseDigit []    = Nothing
stringToParserCaseDigit (h:t) = case runParser (parseInt) (h:t) of
  Just (int, inp)  -> stringToParserAdd (ParserInt int) inp
  Nothing           -> Nothing

stringToParser :: String -> Maybe [ParserAny]
stringToParser [] = Just ([])
stringToParser (h:t)
  | stringToParserIsEmpty h = stringToParser t
  | stringToParserIsUnique h = stringToParserCaseUnique (h:t)
  | isDigit h = stringToParserCaseDigit (h:t)
  | otherwise = case runParser (parseSome (parseAnyChar stringList)) (h:t) of
    Just (str, inp)   -> stringToParserAdd (ParserString str) inp
    Nothing           -> Nothing
