module Main where
import Art
import Data.Char
import Data.List
import Data.Maybe
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

-- Finds the number of occurences where the pattern matches
findSinglePattern :: String -> String -> Char -> [Char] -> Int
findSinglePattern [] [] _ _ = 1
findSinglePattern [] _  _ _ = 0
findSinglePattern _ [] _ _ = 0
findSinglePattern (dict:dicts) (ptrn:ptrns) charLess usedChars
    | elem dict usedChars && ptrn /= dict = 0 -- If the pattern contains a used char, ignore it
    | dict == charLess && ptrn /= charLess = 0
    | ptrn == '_' = findSinglePattern dicts ptrns charLess usedChars
    | dict == ptrn = findSinglePattern dicts ptrns charLess usedChars
    | otherwise = 0

-- Returns total number of matches of a pattern in the dictionary
findPatternMatches :: [String] -> String -> Char -> [Char] -> Int
findPatternMatches [] _ _ _ = 0
findPatternMatches (a:as) pattern charLess usedChars = 
    (findSinglePattern a pattern charLess usedChars) + findPatternMatches as pattern charLess usedChars

-- Replaces just the first instance of the character
replaceFirstChar :: String -> Char -> String
replaceFirstChar [] _ = ""
replaceFirstChar (a:as) chr
    | a == '_' = chr : as
    | otherwise = a : replaceFirstChar as chr

-- Replaces all '*'s with the specified char
replaceAllChar :: String -> Char -> String
replaceAllChar [] _ = ""
replaceAllChar (a:as) chr
    | a == '*' = chr : replaceAllChar as chr
    | otherwise = a : replaceAllChar as chr

-- Find all patterns
findPatterns :: String -> Char -> [String]
findPatterns str chr
    -- VERY IMPORTANT: The first findPatterns should use the wildcard char, that way we'll default to a wrong answer
    | elem '_' str = (findPatterns (replaceFirstChar str '*') chr) ++ (findPatterns (replaceFirstChar str chr) chr)
    | otherwise = [str]

ifExists :: String -> [Char] -> Bool
ifExists _ [] = False
ifExists str (a:as) 
    | elem a str = True
    | otherwise = ifExists str as

-- Cheat at hangman
-- Dictionary, Used Characters Current Word, Specified Char, (New word, Words left)
cheatAtHangman :: [String] -> [Char] -> String -> Char -> (String, Int)
cheatAtHangman dictionary usedChars currentWord specifiedChar = (maximumGroupWord, maxGroup) where
    -- Find every possibility
    allPossibilities = [replaceAllChar x '_' | x <- findPatterns currentWord specifiedChar]
    -- Find the number of pattern matches for each possibility
    patternMatches = [findPatternMatches dictionary x specifiedChar usedChars | x <- allPossibilities ]
    -- Find the maximum item
    maxGroup = maximum patternMatches
    -- Find its index
    maxIndex = fromMaybe 0 $ findIndex (==maxGroup) patternMatches
    -- Find the word
    maximumGroupWord = allPossibilities !! maxIndex

-- Finds a word that matches the pattern
findWordWithPattern :: [String] -> String -> Char -> [Char] -> String
findWordWithPattern (dict:dicts) pattern charLess usedChars
    | findSinglePattern dict pattern charLess usedChars > 0 = dict
    | otherwise = findWordWithPattern dicts pattern charLess usedChars

-- Processes the user input when playing hangman
processUserInput :: [String] -> String -> [Char] -> Int -> Bool -> Char -> IO ()
processUserInput dictionary currentWord usedChars guesses debug letter = do
        -- Call the cheat function and get the next word and size of group
        let (nextWord, nextGroup) = cheatAtHangman dictionary usedChars currentWord letter

        -- Check if word is finished
        if elem '_' nextWord then
            return ()
        else do
            putStrLn $ "You solved it! The word is: " ++ nextWord
            exitSuccess
        
        -- Check for right/wrong guesses
        if nextWord == currentWord then
            putStrLn "Wrong guess!"
        else
            putStrLn "Correct guess!"

        -- Update the guess count
        let nextGuesses = do
            if nextWord == currentWord then
                guesses - 1
            else
                guesses
                
        -- Print the current position of hangman
        putStrLn $ getFail nextGuesses
        
        -- Check for failure condition
        if nextGuesses == 0 then do
            putStrLn "You're out of guesses!"
            putStrLn $ "The word was: " ++ (findWordWithPattern dictionary currentWord letter usedChars)
            exitSuccess
        else
            return ()
        
        -- Let user know what's been finished so far
        putStrLn $ "You have " ++ (show nextGuesses) ++ " guesses left"

        putStrLn $ "Current word is: " ++ nextWord

        -- If debugging, print the words left in a group
        if debug then
            putStrLn $ "DEBUG: There are " ++ (show nextGroup) ++ " words left that fit"
        else
            return ()
        
        -- Prompt user for next input and let them know what's been used
        putStrLn $ "Current used chars are: " ++ (sort (letter : usedChars))
        nextChar <- getNextChar (letter : usedChars)

        -- Do this again
        processUserInput dictionary nextWord (letter : usedChars) nextGuesses debug $ toUpper nextChar

-- Initializes a word given the word length
initializeWord :: Int -> String
initializeWord a
    | a == 0 = ""
    | otherwise = "_" ++ initializeWord (a - 1)

-- Gets the user's next guess
getNextChar :: [Char] -> IO Char
getNextChar usedChars = do
    putStrLn "What's your guess?"
    userLine <- getLine
    if null userLine then do
        putStrLn "You didn't say anything"
        getNextChar usedChars
    else do
        let userChar = head userLine
        if not $ isAlpha userChar then do
            putStrLn "This is not a letter"
            getNextChar usedChars
        else do
            if elem (toUpper userChar) usedChars then do
                putStrLn "You already used this letter"
                getNextChar usedChars
            else
                return (userChar)

-- Removes all words of incorrect word length
removeWrongLength :: [String] -> Int -> [String]
removeWrongLength [] _ = []
removeWrongLength (a:as) specifiedLen
    | length a == specifiedLen = [a] ++ (removeWrongLength as specifiedLen)
    | otherwise = removeWrongLength as specifiedLen

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
    -- We're using the dictionary words as a list of words, so let's make a variable for it
    let dictionaryList = map (map toUpper) (words dictionaryWords)

    -- Check all words in dictionary to ensure there are words of correct length
    if elem wordLength $ map length dictionaryList  then
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

    let startWord = (initializeWord wordLength)
    putStrLn $ "The word is: " ++ startWord
    guessedChar <- getNextChar ""
    processUserInput (removeWrongLength dictionaryList wordLength) startWord "" guesses debug $ toUpper guessedChar

    putStrLn "Done"