
module HoldMeeting where

-- Hold meeting function to return tuple
holdMeeting :: String -> (Int, Bool)
holdMeeting a =
    -- Return tuple for total available and whether it's enough
    (total, total >= min) where
    -- Define variables
    nums = words a
    min = read (nums !! 0)::Int
    delay = read (nums !! 1)::Int
    total = determineGood delay (map read (drop 2 nums))

-- Function used to calculate the number of people available for the meeting
determineGood :: Int -> [Int] -> Int
determineGood _ [] = 0 -- If list is empty, return 0
determineGood delay (a:as)
    -- If next index is good, return 1 plus everything after index
    | a <= delay = 1 + determineGood delay as
    -- Otherwise return everything after index
    | otherwise = determineGood delay as