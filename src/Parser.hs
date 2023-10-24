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
  , StringToParserOutput
  , isolateStringToParserOutput
  , isolateStringToParserStatus
  , runParser
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

import ParserStatus (
  ParserStatus,
  createParserStatusErrorSimple,
  createParserStatusOk
  )

import Data.Char (isDigit)

--type Parser a = String -> Maybe (a, String)

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

type StringToParserOutput = (Maybe [ParserAny], ParserStatus)

isolateStringToParserOutput :: StringToParserOutput -> Maybe [ParserAny]
isolateStringToParserOutput (output, _) = output

isolateStringToParserStatus :: StringToParserOutput -> ParserStatus
isolateStringToParserStatus (_, status) = status

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

-- UNUSED
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


-- UNUSED
parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 = parseAndWith (,) p1 p2

-- UNUSED
parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f p1 p2 =
  Parser $ \input ->
    case runParser p1 input of
      Just (result1, remaining1) ->
        case runParser p2 remaining1 of
          Just (result2, remaining2) -> Just (f result1 result2, remaining2)
          Nothing                    -> Nothing
      Nothing -> Nothing


-- UNUSED
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

-- UNUSED
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

-- UNUSED
parseListSegment :: Parser a -> [a] -> Parser [a]
parseListSegment parser list =
  Parser $ \input1 -> do
    (element, input2) <- runParser parser input1
    case runParser (parseChar ' ') input2 of
      Just (_, b) -> runParser (runParseListSegment (list ++ [element])) b
      Nothing     -> Just ((list ++ [element]), input2)
    where
      runParseListSegment = parseListSegment parser

-- UNUSED
parseList :: Parser a -> Parser [a]
parseList parser =
  Parser $ \input1 -> do
    (_, input2) <- runParser (parseChar '(') input1
    (list, input3) <- runParser (parseListSegment parser []) input2
    case runParser (parseChar ')') input3 of
      Just (_, input4)  -> Just (list, input4)
      Nothing           -> Nothing

-- STRING TO PARSER --

stringToParserAdd :: ParserAny -> String -> StringToParserOutput
stringToParserAdd element string = case stringToParser string of
  (Just list, parserStatus) -> (Just (element:list), parserStatus)
  (Nothing, parserStatus)   -> (Nothing, parserStatus)

stringToParserIsEmpty :: Char -> Bool
stringToParserIsEmpty ' ' = True
stringToParserIsEmpty '\n' = True
stringToParserIsEmpty _ = False

stringToParserIsUnique :: Char -> Bool
stringToParserIsUnique '(' = True
stringToParserIsUnique ')' = True
stringToParserIsUnique _ = False

stringListOperators :: [Char]
stringListOperators = "+-*/%<>=?!#"

stringList :: [Char]
stringList = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ stringListOperators

stringToParserCaseUnique :: String -> StringToParserOutput
stringToParserCaseUnique []    = (Nothing,
  createParserStatusErrorSimple "stringToParserCaseUnique" "")
stringToParserCaseUnique (h:t) = case runParser (parseChar h) (h:t) of
  Just (char, inp)  -> stringToParserAdd (ParserChar char) inp
  Nothing           -> (Nothing,
    createParserStatusErrorSimple "stringToParserCaseUnique" "")

stringToParserCaseDigit :: String -> StringToParserOutput
stringToParserCaseDigit []    = (Nothing,
  createParserStatusErrorSimple "stringToParserCaseDigit" "")
stringToParserCaseDigit (h:t) = case runParser (parseInt) (h:t) of
  Just (int, []) -> stringToParserAdd (ParserInt int) []
  Just (int, (inpH:inpT)) ->
    case or [stringToParserIsEmpty inpH, stringToParserIsUnique inpH] of
      True  -> stringToParserAdd (ParserInt int) (inpH:inpT)
      False -> (Nothing,
        createParserStatusErrorSimple "stringToParserCaseDigit" "")
  Nothing                 -> (Nothing,
    createParserStatusErrorSimple "stringToParserCaseDigit" "")

stringToParser :: String -> StringToParserOutput
stringToParser [] = (Just [], createParserStatusOk)
stringToParser (h:t)
  | stringToParserIsEmpty h = stringToParser t
  | stringToParserIsUnique h = stringToParserCaseUnique (h:t)
  | isDigit h = stringToParserCaseDigit (h:t)
  | otherwise = case runParser (parseSome (parseAnyChar stringList)) (h:t) of
    Just (str, inp)   -> stringToParserAdd (ParserString str) inp
    Nothing           -> (Nothing,
      createParserStatusErrorSimple "stringToParser" "")
