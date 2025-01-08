module Cards where -- creating the module

import Utils
import Control.Monad (replicateM)

all_cards :: [String] = [
    "Ace", "Ace", "Ace", "Ace",
    "2", "2", "2", "2",
    "3", "3", "3", "3", 
    "4", "4", "4", "4", 
    "5", "5", "5", "5", 
    "6", "6", "6", "6",
    "7", "7", "7", "7",
    "8", "8", "8", "8",
    "9", "9", "9", "9", 
    "10", "10", "10", "10",
    "K", "K", "K", "K",
    "Q", "Q", "Q", "Q",
    "J", "J", "J", "J"
    ]

cardValueToINT :: String -> Int -- this functions get the card and returns its value based on Blackjack rules
cardValueToINT card
    | card == "J" || card == "Q" || card == "K" = 10 -- this is an if else block, | -> if statemtn, || -> OR, otherwise means the last else
    | card == "Ace" = 11
    | otherwise = case strToInt card of -- here is like a try catch block,
                    Just number -> number -- i don't care about fails, I just get the value if there is

-- Fisher-Yates algorithm
shuffleCards :: [String] -> IO [String]
shuffleCards [] = return [] -- if the list is empty, we return an empty list
shuffleCards arr = do
    rngIdx <- getRNG(length arr - 1) -- We use the getRNG function to get a random index, having boundaries set to 0 <-> length - 1
    let (before, x:after) = splitAt rngIdx arr -- before, and after are 2 lists, 1 list from 0 to specified x, which is the rngIdx, and after is from idx to length - 1

    rest <- shuffleCards (before ++ after) -- ++ concatenates 2 lists, we keep x on place, the selected index and re call shuffleCards again with the new list
    return (x : rest) -- add the selected item to the first item, basically selecting items and pushing it to the first position


removeFirstCard :: [String] -> [String]
removeFirstCard [] = []  -- if we get an empty list, return an empty list
removeFirstCard (_:xs) = xs  -- syntax _:xs ignores the first item of the list