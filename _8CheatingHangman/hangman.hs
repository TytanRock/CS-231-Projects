module Main where
import Data.Char
import System.Directory
import System.Environment
import System.IO
import System.Exit

-- Variable to hold the usage arguments
argumentUsage = "Usage: ./hangman <dictionary file> <word length> <guesses>"

-- Check if every character in a string is a number
isNum :: String -> Bool
isNum [] = True
isNum (a:as)
    | isDigit a = isNum as
    | otherwise = False

-- Validates that the arguments are of correct usage
validateCommandArgs :: [String] -> (Bool, String)
validateCommandArgs strs
    | length strs < 3 = (True, argumentUsage)
    | not $ isNum (strs !! 1) = (True, "Incorrectly formatted: " ++ argumentUsage)
    | not $ isNum (strs !! 2) = (True, "Incorrectly formatted: " ++ argumentUsage)
    | otherwise = (False, "")

-- Parses arguments into parameters
parseCommandArgs :: [String] -> (String, Int, Int, Bool)
parseCommandArgs strs = (fileName, wordLength, guesses, debug) where
    fileName = strs !! 0
    wordLength = read (strs !! 1)
    guesses = checkGuessCount $ read (strs !! 2)
    debug = (length strs > 3) && (strs !! 3) == "-s"

-- Fixes guess count if incorrect
checkGuessCount :: Int -> Int
checkGuessCount n
    | n < 5 = 5
    | n > 10 = 10
    | otherwise = n

-- Main Program
main = do
    commandArguments <- getArgs
    -- Check if user used command correctly
    let (exitProgram, exitMessage) = validateCommandArgs commandArguments
    if exitProgram then do
        putStrLn exitMessage
        exitFailure -- Exit program if incorrect
    else
        return () -- Empty statement cause it's haskell
    
    exists <- doesFileExist (commandArguments !! 0)
    if not exists then do
        putStrLn "Dictionary File does not exist"
        exitFailure  -- Exit program if file doesn't exist
    else
        return () -- Empty statement cause it's haskell
    
    -- At this point the arguments have been cleaned

    -- Now let's get the parameters the user specified
    let (fileName, wordLength, guesses, debug) = parseCommandArgs commandArguments

    -- Get the contents of the dictionary
    dictionaryHandle <- openFile fileName ReadMode
    dictionaryWords <- hGetContents dictionaryHandle

    -- Check all words in dictionary to ensure there are words of correct length
    if elem wordLength $ map length $ words dictionaryWords then
        -- There exists at least one word with the specied length
        return ()
    else do
        -- There does not exist a word of the specified length
        putStrLn "Invalid word length, there are no words of that length!"
        exitFailure

    -- Print number of guesses available
    putStrLn $ "Guesses are: " ++ show guesses    

    -- If in debug mode, let user know
    if debug then
        putStrLn "Oh ho Ho! You're using the debug function!"
    else
        return ()

    putStrLn "Done"