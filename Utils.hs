module Utils where -- creating the module
import Text.Read (readMaybe) -- import the readMaybe for type convertion

import System.Random (randomRIO) -- import the random module by using () you can choose what function to import

getRNG :: Int -> IO Int -- we get an Int as a parameter and return an Int as well
getRNG range = randomRIO (0, range) -- here we name the variable, in this case range and also return it with the value of randomRIO (0, range), meaning the rng value will be between 0, range theshold

strToInt :: String -> Maybe Int -- convertsion for String type to Int type, maybe handles the cases in which the conversion fails
strToInt = readMaybe -- the conversion syntax 

fancyPrintGameStatus :: String -> IO() -- just for UI purpose, dipslay the game status like so
fancyPrintGameStatus str = do
    putStrLn ("\n#=#=#=#=#=#=#=#=#=#=#=#=#\n >>> " ++ str ++ " <<< " ++ "\n#=#=#=#=#=#=#=#=#=#=#=#=#")