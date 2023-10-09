{-
-- EPITECH PROJECT, 2023
-- ParserStatus
-- File description:
-- ParserStatus
-}

module ParserStatus
  (
    ParserStatus (ParserStatusError, ParserStatusOK),
    createParserStatusError,
    createParserStatusErrorGeneric,
    createParserStatusErrorSimple,
    createParserStatusOk,
    interpretParserStatus,
    isParserStatusError,
    isParserStatusOk
  ) where

-- DATA --

data ParserStatus =
  ParserStatusOK | ParserStatusError String String Int Int
  deriving (Show)

-- CREATING PARSERSTATUS --

createParserStatusOk :: ParserStatus
createParserStatusOk = ParserStatusOK

createParserStatusErrorGeneric :: ParserStatus
createParserStatusErrorGeneric = ParserStatusError "" "" 0 0

createParserStatusErrorSimple :: String -> String -> ParserStatus
createParserStatusErrorSimple name info = ParserStatusError name info 0 0

createParserStatusError :: String -> String -> Int -> Int -> ParserStatus
createParserStatusError name info ln col = ParserStatusError name info ln col

-- IS OF TYPE --

isParserStatusOk :: ParserStatus -> Bool
isParserStatusOk ParserStatusOK = True
isParserStatusOk _ = False

isParserStatusError :: ParserStatus -> Bool
isParserStatusError (ParserStatusError _ _ _ _) = True
isParserStatusError _ = False

-- INTERPRET --

interpretParserStatusErrorName :: String -> String
interpretParserStatusErrorName "" = "Unknown Error"
interpretParserStatusErrorName name = name

interpretParserStatusErrorInfo :: String -> String
interpretParserStatusErrorInfo "" = ""
interpretParserStatusErrorInfo name = ": " ++ name

interpretParserStatusErrorLnCol :: Int -> Int -> String
interpretParserStatusErrorLnCol 0 0 = ""
interpretParserStatusErrorLnCol col ln =
  " (Ln " ++ (show col) ++ ", Col " ++ (show ln) ++ ")"

interpretParserStatus :: ParserStatus -> String
interpretParserStatus (ParserStatusError name info ln col) =
  interpretParserStatusErrorName name ++ interpretParserStatusErrorInfo info ++
  interpretParserStatusErrorLnCol ln col
interpretParserStatus ParserStatusOK = "OK"
