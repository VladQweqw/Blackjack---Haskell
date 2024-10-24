import System.Random (randomRIO) -- import the random module
import Text.Read (readMaybe) -- import the readMaybe for type convertion

cards :: [String] = ["Ace", "2", "3", "4", "5", "6", "7", "8", "9", "10", "K", "Q", "J"] -- create the pack in which we put the cards of type string because is an array of string, strings because ace has 3 letters not 1. ( char )

main :: IO () -- main fnc 
main = do
    putStrLn ( -- estetic print
        "*=-=-=-=-=-= Blackjack =-=-=-=-=-=*\n" ++
        "|                                 |\n" ++
        "|     Press [ENTER] to start!     |\n" ++
        "|                                 |\n" ++
        "*=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*")
    input <- getLine -- get the user response

    if input == "" then -- if the user presses enter
        startGame -- call the startGame fnc
    else
        putStrLn "Maybe next time.." -- an exit message

getRNG :: Int -> IO Int -- we get an Int as a parameter and return an Int as well
getRNG range = randomRIO (0, range) -- here we name the variable, in this case range and also return it with the value of randomRIO (0, range), meaning the rng value will be between 0, range theshold

getCard :: Bool -> Int -> IO String -- we have 2 parameters , one is of type Bool -> Int and return String
getCard isPlayer deckSum = do -- variable naming
    rng <- getRNG (length cards - 1) -- here we set the rng variable value to a random number with a range from 0 - (length of the card - 1), basically including any card dynamically
    let rngCard = cards !! rng -- this in other languages means cards[rng], we are selecting a random card from the pack

    if rngCard == "Ace" then do -- if the card happenens to be an Ace then
        if isPlayer then do -- if the card request comes from the user then
            putStrLn "!!!! You got an Ace, keep it 11 [PRESS ENTER] or change it to 1 [TYPE ANYTHING]? !!!!" -- Notification string

            if deckSum + 11 > 21 then do -- some hint for the user, if the deck value is higher than 21 with the ace's value 11 we give the user a hint on what to choose.
                putStrLn ("We reccomend change the Ace to 1, otherwise your deck will be " ++ show (deckSum + 11)) -- show converts INT to String ( apparently )
            else
                putStrLn ("We recommend change the Ace to 11, the deck value will be " ++ show (deckSum + 11))

            keepItEleven <- getLine -- get the user option

            if keepItEleven == "" then do -- if so user choose the ace to be 11, [ENTER], we return the value "Ace" and notify the user about its choice, we return "Ace" as a string an not a number because we handle counting to another function
                putStrLn ">>>Your ace will be counted as 11"
                return "Ace"
            else do
                putStrLn ">>>Your ace will be counted as 1" -- same thing
                return "1"
        else do -- if the request comes from the robot
            if deckSum == 0 then -- basically not the the user when the hidden card of the robot is an ace, look at getInitialDeck fnc to understand, basically if the cardSum is 0, make the Ace directly 11 for a better gameplay-
                return "Ace" -- return the card
            else do
                putStrLn "!!!! The robot got an Ace !!!!"
                if deckSum + 11 > 21 then do -- just some hints for the user, it automatically calculates the deck value with the ace as 11, in order to tell the user the best choice
                    putStrLn "The robot choose the Ace to be 1"
                    return "1"
                else do
                    putStrLn "The robot choose the Ace to be 11"
                    return "Ace"
    else
        return rngCard -- if the card is not an ace, simple, just return it

getInitialDeck :: Bool -> Int -> IO [String] -- this function is used to get the first 2 cards for the robot and user, and it returns an array of strings
getInitialDeck isPlayer deckSum = do
    if deckSum == 1001 then do -- IF the robot got 2 aces, 1 will be 11 another one will be 1, in order to not lose instaltenly, 1001 is the discriminator between user and robot, if decksum is 1001 which is impossible in a normal gameplay, we know it's the robot
        firstCard <- getCard isPlayer 0 -- first deck value will be 0 because in case there is an Ace, we want it to be 11
        secondCard <- getCard isPlayer 11 -- the second deck value will be 11, because if there happens to be 2 aces, we want the second one to be 1, total to be 12 not 22 to lose
        return [firstCard, secondCard] -- return the cards
    else do
        firstCard <- getCard isPlayer deckSum -- same thing but pass the deckSum parameter, not an absolute value
        secondCard <- getCard isPlayer deckSum
        return [firstCard, secondCard]

displayDecks :: [String] -> [String] -> Bool -> IO () -- get 2 string arrays ( decks ) and a bool, then return some prints so we use io()
displayDecks playerDeck robotDeck showRobot = do
    putStr ("+-----------------------+\n" ++ -- just for UI&UX purpose
            "| User deck: ")
    let playerConcat = foldr (\card acc -> card ++ " " ++ acc) "" playerDeck -- here we basically from [1,2,3] we prin 1 2 3,
    -- foldr is like for or reduce() backwards in JS, fold + r, r stands for right, so it iterate from right to left
    -- card is the current card, in this sequence 3 -> 2 -> 1
    -- acc in my case stands from accumulator, it basically appends to it each letter and a space, 
    -- \ -> is a lambda function, which has 2 arguments, in this case acc and card, and returns card ++ " " ++ acc, in other words 3 2 1
    -- The "" after the () refers to the inital value of the accumulator, in this case I want it to be a string to append to it and have spaces
    -- playerDeck is the array I'm looping over                   
    -- keep in mind card / acc are just variable names
    putStr playerConcat -- print the playerContac which is a string

    if showRobot then do -- now we want to check who's cards we are displaying, if is the user we don't have any restrictions, if it's the robot we have to hide the first element, in this case is a ?
        putStr "\n| Robot deck: " -- this is used when the game ends, we want to show all the cards 
        let robotConcat = foldr (\card acc -> card ++ " " ++ acc) "" robotDeck -- same as before
        putStr robotConcat
    else do
        putStr "\n| Robot deck: ? " -- display when the game is playing
        let robotConcat = foldr (\card acc -> card ++ " " ++ acc) "" (tail robotDeck) -- tail -> primeste o lista si returneaza lista fara primul element, ( exact ce ne trebuie pe scurt)
        putStr robotConcat

    putStr "\n+-----------------------+"
    putStr "\n>>> [ENTER] Hit | [TYPE ANYTHING] Stay\n" -- UI&UX

strToInt :: String -> Maybe Int -- convertsion for String type to Int type, maybe handles the cases in which the conversion fails
strToInt = readMaybe -- the conversion syntax 

cardValueToINT :: String -> Int -- this functions get the card and returns its value based on Blackjack rules
cardValueToINT card
    | card == "J" || card == "Q" || card == "K" = 10 -- this is an if else block, | -> if statemtn, || -> OR, otherwise means the last else
    | card == "Ace" = 11
    | otherwise = case strToInt card of -- here is like a try catch block,
                    Just number -> number -- i don't care about fails, I just get the value if there is

calculateDeck :: [String] -> Int 
calculateDeck deck = sum (map cardValueToINT deck) -- we calculate de deck value using sum (), we give sum the arguments of (map cardValueToInt deck) which means it returns an array of numbers,

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
    robotCards <- getInitialDeck False 1001

    displayDecks playerCards robotCards False
    whileLoop playerCards robotCards False




