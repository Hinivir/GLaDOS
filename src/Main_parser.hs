{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Main_parser
-}

-- function errorExit
-- Take a String
-- Prints it on error output with exitWith 84 (for indicates an error)
errorExit :: String -> IO ()
errorExit msg = hPutStrLn stderr msg >> exitWith (ExitFailure 84)


-- function mainCheckEOF
-- Take a Boolean
-- If true: then calls errorExit with "No input"
-- If false: use getArgs to send input to chainedMap then mainLoop
mainCheckEOF :: Bool -> IO ()
mainCheckEOF True = errorExit "No input"
mainCheckEOF False = getArgs >>= chainedMap
                     >>= (\ listA -> mainLoop listA [])


-- function main
-- Take IO (it's the main)
-- If no standard input then error
-- Otherwise passes standard input to the rest of the program
main :: IO ()
main = isEOF >>= mainCheckEOF
