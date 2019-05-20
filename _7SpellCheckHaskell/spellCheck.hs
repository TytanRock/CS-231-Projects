module Main where
import Data.Char
import Data.List
import System.Environment
import System.IO

-- Split the string based on non-alphabetic characters
getWords :: String -> [String]
getWords [] = [] -- Empty string returns empty string
getWords [a]
    | isAlpha a = [[a]] -- String of only one character returns list of that character as a string
    | otherwise = [""]
getWords (a:b:abs) -- All else will run through the algorithm
    | not $ isAlpha a = getWords (b:abs) -- If the first character is not alphabetic, get the remainder
    | otherwise = do
        if isAlpha b then do -- If the second character is alphabetic, continue the string
            let remainder = getWords (b : abs)
            [a : (remainder !! 0)] ++ tail remainder
        else do
            let next = getWords abs -- Otherwise split the string
            if next == [""] then
                [[a]]
            else
                [[a]] ++ next

-- Removes duplicate words
removeLikeWords :: [String] -> [String]
removeLikeWords [] = []
removeLikeWords (a:as)
    -- If there exists a (lowercase) element in the rest of the (lowercase) list, ignore it
    | elem (map toLower a) (map (map toLower) as) = removeLikeWords as
    | otherwise = a : removeLikeWords as

-- Ordering function to order strings based without case sensitivity
caseInsensitiveOrdering :: String -> String -> Ordering
caseInsensitiveOrdering a b
    | (map toLower a) < (map toLower b) = LT
    | otherwise = GT

-- Puts all the functions together into one line
processWords :: String -> [String]
processWords str = sortBy (caseInsensitiveOrdering) (removeLikeWords $ getWords str)

-- Will check every item in the string list with every item in the other string list
checkSpelling :: [String] -> [String] -> [String]
checkSpelling [] _ = []
checkSpelling (str:strs) dic
    | elem (map toLower str) (map (map toLower) dic) = (str ++ ":\tcorrect") : checkSpelling strs dic
    | otherwise = (str ++ ":\tnot correct") : checkSpelling strs dic

-- Main program
main = do
    -- Get the filenames for each file
    [dictionaryFile, inFile, outFile] <- getArgs
    dictionaryHandle <- openFile dictionaryFile ReadMode -- Open file in read mode
    inFileHandle <- openFile inFile ReadMode -- Open file in read mode
    outFileHandle <- openFile outFile WriteMode -- Open file in write mode

    inFileContents <- hGetContents inFileHandle -- Get the contents of file
    dicFileContents <- hGetContents dictionaryHandle -- Get the contents of file

    let allWords = processWords inFileContents -- Process the words in the first file
    let spellCheckedWords = checkSpelling allWords $ words dicFileContents -- Check them agains the dictionary
    
    hPutStrLn outFileHandle $ unlines spellCheckedWords -- Output results to output file

    -- Close files
    hClose dictionaryHandle
    hClose inFileHandle
    hClose outFileHandle
    