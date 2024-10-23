import System.Random (randomRIO)
import Data.List (intercalate)
import Text.Read (readMaybe)

cards :: [String] = ["Ace", "2", "3"]
-- cards :: [String] = ["Ace", "2", "3", "4", "5", "6", "7", "8", "9", "10", "K", "Q", "J"]

main :: IO ()
main = do
    putStrLn (
        "*=-=-=-=-=-= Blackjack =-=-=-=-=-=*\n" ++
        "|                                 |\n" ++
        "|     Press [ENTER] to start!     |\n" ++
        "|                                 |\n" ++
        "*=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*")
    input <- getLine -- Reads input

    if input == "" then
        startGame
    else
        putStrLn "Maybe next time.."

getRNG :: Int -> IO Int
getRNG range = randomRIO (0, range)

getCard :: Bool -> Int -> IO String
getCard isPlayer deckSum = do
    rng <- getRNG (length cards - 1)
    let rngCard = cards !! rng

    print (rngCard, isPlayer, deckSum)
    if rngCard == "Ace" then do
        
        if isPlayer then do
            putStrLn "!!!! You got an Ace, keep it 11 [PRESS ENTER] or change it to 1 [TYPE ANYTHING]? !!!!"

            if deckSum + 11 > 21 then do
                putStrLn ("We reccomend change the Ace to 1, otherwise your deck will be " ++ show (deckSum + 11)) -- show converts INT to String ( apparently )
            else
                putStrLn ("We recommend change the Ace to 11, the deck value will be " ++ show (deckSum + 11))

            input <- getLine -- Reads input

            if input == "" then do
                putStrLn ">>>Your ace will be counted as 11"
                return "Ace"
            else do
                putStrLn ">>>Your ace will be counted as 1"
                return "1"
        else do
            if deckSum == 0 then -- basically not the the user when the hidden card of the robot is an ace
                return "Ace"
            else do
                putStrLn "!!!! The robot got an Ace !!!!"
                if deckSum + 11 > 21 then do
                    putStrLn "The robot choose the Ace to be 1"
                    return "1"
                else do
                    putStrLn "The robot choose the Ace to be 11"
                    return "Ace"
    else
        return rngCard

getInitialDeck :: Bool -> Int -> IO [String]
getInitialDeck isPlayer deckSum= do
    firstCard <- getCard isPlayer deckSum
    secondCard <- getCard isPlayer 1001
    return [firstCard, secondCard]

displayDecks :: [String] -> [String] -> Bool -> IO ()
displayDecks playerDeck robotDeck showRobot = do
    putStr ("+-----------------------+\n" ++
            "| User deck: ")
    let playerConcat = foldr (\card acc -> card ++ " " ++ acc) "" playerDeck
    putStr playerConcat

    if showRobot then do
        putStr "\n| Robot deck: "
        let robotConcat = foldr (\card acc -> card ++ " " ++ acc) "" robotDeck
        putStr robotConcat
    else do
        putStr "\n| Robot deck: ? "
        let robotConcat = foldr (\card acc -> card ++ " " ++ acc) "" (tail robotDeck)
        putStr robotConcat

    putStr "\n+-----------------------+"
    putStr "\n>>> [ENTER] Hit | [TYPE ANYTHING] Stay\n"

strToInt :: String -> Maybe Int
strToInt = readMaybe

cardValueToINT :: String -> Int
cardValueToINT card
    | card == "J" || card == "Q" || card == "K" = 10
    | card == "Ace" = 11
    | otherwise = case strToInt card of
                    Just number -> number

calculateDeck :: [String] -> Int
calculateDeck deck = sum (map cardValueToINT deck)

isGameOver :: [String] -> [String] -> Bool -> String
isGameOver playerDeck robotDeck isStay
    | isStay && playerSum > robotSum = "Player wins"
    | isStay && playerSum < robotSum = "Robot wins"
    | isStay && playerSum == robotSum = "DRAW"

    | playerSum > 21 && robotSum > 21 = "DRAW"
    | playerSum >= 21 = "Robot wins"
    | robotSum >= 21 = "Player wins"
    | playerSum == 21 = "Player wins with 21"
    | robotSum == 21 = "Robot wins with 21"
    | otherwise = "Game continues"
    where
        playerSum = calculateDeck playerDeck
        robotSum = calculateDeck robotDeck

fancyPrintGameStatus :: String -> IO()
fancyPrintGameStatus str = do
    putStrLn ("\n#=#=#=#=#=#=#=#=#=#=#=#=#\n >>> " ++ str ++ " <<< " ++ "\n#=#=#=#=#=#=#=#=#=#=#=#=#")

whileLoop :: [String] -> [String] -> Bool -> IO()
whileLoop playerCards robotCards gameOver
    | gameOver = return ()
    | otherwise = do
            input <- getLine -- Reads input

            if input == "" then do
                playerNewCard <- getCard True (calculateDeck playerCards)
                let newPlayerCards = playerCards ++ [playerNewCard]

                robotNewCard <- getCard False  (calculateDeck robotCards)
                let newRobotCards = robotCards ++ [robotNewCard]

                let gameStatus = isGameOver newPlayerCards newRobotCards False

                if gameStatus == "Game continues" then do
                    displayDecks newPlayerCards newRobotCards False
                    whileLoop newPlayerCards newRobotCards False
                else do
                    displayDecks newPlayerCards newRobotCards True
                    fancyPrintGameStatus gameStatus;
            else do
                displayDecks playerCards robotCards True
                fancyPrintGameStatus (isGameOver playerCards robotCards True)

startGame :: IO ()
startGame = do
    playerCards <- getInitialDeck True 0
    robotCards <- getInitialDeck False 0

    displayDecks playerCards robotCards False
    whileLoop playerCards robotCards False




