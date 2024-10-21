import System.Random (randomRIO)
import Data.List (intercalate)

cards :: [String] = ["Ace", "2", "3", "4", "5", "6", "7", "8", "9", "10"]

main :: IO ()
main = do
    putStrLn "=-=-=-=-=-= Blackjack =-=-=-=-=-=\n   >>>Press [ENTER] to start!"
    input <- getLine -- Reads input

    if input == "" then
        startGame
    else
        putStrLn "Maybe next time.."

getRNG :: Int -> IO Int
getRNG range = randomRIO (1, range)

-- removeItem :: String -> [String] -> [String]
-- removeItem cardToDelete remainingCards = [x | x <- remainingCards, x /= cardToDelete]

getCard :: IO String
getCard = do
    rng <- getRNG (length cards - 1)
    let rngCard = cards !! rng
    return rngCard

getInitialDeck :: IO [String]
getInitialDeck = do
    firstCard <- getCard
    secondCard <- getCard
    return [firstCard, secondCard]

displayDecks :: [String] -> [String] -> IO ()
displayDecks playerDeck robotDeck = do
    putStr "User deck: "
    let playerConcat = foldr (\card acc -> card ++ " " ++ acc) "" playerDeck
    putStr playerConcat

    putStr "\nRobot deck: "
    let robotConcat = foldr (\card acc -> card ++ " " ++ acc) "" robotDeck
    putStr robotConcat
    putStr "\n[ENTER] Hit | [SHIFT] Pass"


startGame :: IO ()
startGame = do
    playerCards <- getInitialDeck
    robotCards <- getInitialDeck
    
    displayDecks playerCards robotCards

