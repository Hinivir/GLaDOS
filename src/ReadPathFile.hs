{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- ReadPathFile.hs
-}

module ReadPathFile(
    fileToList,
    loopFileToList
) where

import System.IO
    ( hClose, hIsEOF, hGetLine, openFile, Handle, IOMode(ReadMode) )

-- function fileToList
-- Take the file path
-- Return a list of String with lines of the file and close the file
fileToList :: FilePath -> IO [String]
fileToList path = do
  handle <- openFile path ReadMode
  listString <- loopFileToList handle []
  hClose handle
  return listString


-- function loopFileToList
-- Take the content of the file, processes it recursively
-- Return a list of String with lines of the file
loopFileToList :: Handle -> [String] -> IO [String]
loopFileToList handle lineStock = do
  eof <- hIsEOF handle
  if eof
    then return lineStock
    else do
      line <- hGetLine handle
      let updatedLines = lineStock ++ [line]
      loopFileToList handle updatedLines


{-
-- function main for test
main :: IO ()
main = do
  let filePath = "test.txt"
  lines <- fileToList filePath
  mapM_ putStrLn lines
-}
