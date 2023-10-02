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
--  , stringToParser
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


-- | Parse a list of char in a string
--
-- Returns 'Nothing' if ther is not the char in the string.
-- Returns 'Just' the char and the rest of the string.
--parseAnyChar (x:xs) (y:ys)
--parseAnyChar :: String -> Parser Char
--  | x == y = Just (y, ys)
--  | otherwise = parseAnyChar xs (y:ys)
--parseAnyChar _ _ = Nothing
parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 =
  Parser $ \input ->
    case runParser p1 input of
      Just (result, remaining) -> Just (result, remaining)
      Nothing -> runParser p2 input



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
--parsePair parser1 parser2 =
--  Parser $ \input -> do
--    (_, input1) <- runParser (parseChar '(') input
--    (y, input2) <- runParser parser1 input1
--    (_, input3) <- runParser (parseChar ',') input2
--    (z, input4) <- runParser parser2 input3
--    (_, input5) <- runParser (parseChar ')') input4
--    return ((y, z), input5)
parsePair _ = Parser $ const Nothing



parseList :: Parser a -> Parser [a]
parseList parser =
  Parser $ \input ->
    case runParser (parseMany parser) input of
      Just (results, remaining) -> Just (results, remaining)
      Nothing                   -> Nothing

--stringToParser :: String -> Parser Char
--stringToParser _ = Nothing
--stringToParser