module Main where
import Data.Char
import Data.List
import System.Environment
import System.IO

getWords :: String -> [String]
getWords [] = []
getWords [a] = [[a]]
getWords (a:b:abs) 
    | not $ isAlpha a = getWords (b:abs)
    | otherwise = do
        if isAlpha b then do
            let remainder = getWords (b : abs)
            [a : (remainder !! 0)] ++ tail remainder
        else
            [[a]] ++ getWords abs

removeLikeWords :: [String] -> [String]
removeLikeWords [] = []
removeLikeWords (a:as)
    | elem (map toLower a) (map (map toLower) as) = removeLikeWords as
    | otherwise = a : removeLikeWords as

caseInsensitiveOrdering :: String -> String -> Ordering
caseInsensitiveOrdering a b
    | (map toLower a) < (map toLower b) = LT
    | otherwise = GT

processWords :: String -> [String]
processWords str = sortBy (caseInsensitiveOrdering) (removeLikeWords $ getWords str)

checkSpelling :: [String] -> [String] -> [String]
checkSpelling [] _ = []
checkSpelling (str:strs) dic
    | elem (map toLower str) (map (map toLower) dic) = (str ++ ":\tcorrect") : checkSpelling strs dic
    | otherwise = (str ++ ":\tnot correct") : checkSpelling strs dic

main = do
    [dictionaryFile, inFile, outFile] <- getArgs
    dictionaryHandle <- openFile dictionaryFile ReadMode
    inFileHandle <- openFile inFile ReadMode
    outFileHandle <- openFile outFile WriteMode

    inFileContents <- hGetContents inFileHandle
    dicFileContents <- hGetContents dictionaryHandle

    let allWords = processWords inFileContents
    let spellCheckedWords = checkSpelling allWords $ words dicFileContents
    
    hPutStrLn outFileHandle $ unlines spellCheckedWords

    hClose dictionaryHandle
    hClose inFileHandle
    hClose outFileHandle
    