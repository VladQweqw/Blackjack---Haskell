import System.Random (randomRIO)
import Data.List (intercalate)
import Text.Read (readMaybe)

cards :: [String] = ["A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "K", "Q", "J"]

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
getRNG range = randomRIO (1, range)

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
    | card == "A" = 11
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
                playerNewCard <- getCard
                let newPlayerCards = playerCards ++ [playerNewCard]

                robotNewCard <- getCard
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
    playerCards <- getInitialDeck
    robotCards <- getInitialDeck
    
    displayDecks playerCards robotCards False
    whileLoop playerCards robotCards False




