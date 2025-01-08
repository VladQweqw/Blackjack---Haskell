import Cards -- import the Cards module
import Utils

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

getCard :: [String] -> Bool -> Int -> IO String -- we have 2 parameters , one is of type Bool -> Int and return String
getCard cards isPlayer deckSum = do -- variable naming
    let rngCard = head cards -- get the first item of the list

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
        else do -- if the request comes from the dealer
            if deckSum == 0 then -- basically not the the user when the hidden card of the dealer is an ace, look at getInitialDeck fnc to understand, basically if the cardSum is 0, make the Ace directly 11 for a better gameplay-
                return "Ace" -- return the card
            else do
                putStrLn "!!!! The dealer got an Ace !!!!"
                if deckSum + 11 > 21 then do -- just some hints for the user, it automatically calculates the deck value with the ace as 11, in order to tell the user the best choice
                    putStrLn "The dealer choose the Ace to be 1"
                    return "1"
                else do
                    putStrLn "The dealer choose the Ace to be 11"
                    return "Ace"
    else
        return rngCard -- if the card is not an ace, simple, just return it

getInitialDeck :: [String] -> Bool -> Int -> IO [String] -- this function is used to get the first 2 cards for the dealer and user, and it returns an array of strings
getInitialDeck cards isPlayer deckSum = do
    if deckSum == 1001 then do -- IF the dealer got 2 aces, 1 will be 11 another one will be 1, in order to not lose instaltenly, 1001 is the discriminator between user and dealer, if decksum is 1001 which is impossible in a normal gameplay, we know it's the dealer
        firstCard <- getCard cards isPlayer 0 -- first deck value will be 0 because in case there is an Ace, we want it to be 11
        secondCard <- getCard cards isPlayer 11 -- the second deck value will be 11, because if there happens to be 2 aces, we want the second one to be 1, total to be 12 not 22 to lose
        return [firstCard, secondCard] -- return the cards
    else do
        firstCard <- getCard cards isPlayer deckSum -- same thing but pass the deckSum parameter, not an absolute value
        secondCard <- getCard cards isPlayer deckSum
        return [firstCard, secondCard]

displayDecks :: [String] -> [String] -> Bool -> IO () -- get 2 string arrays ( decks ) and a bool, then return some prints so we use io()
displayDecks playerDeck dealerDeck showRobot = do
    putStr ("+-----------------------+\n" ++ -- just for UI&UX purpose
            "| User's deck: ")
    let playerConcat = foldr (\card acc -> card ++ " " ++ acc) "" playerDeck -- here we basically from [1,2,3] we prin 1 2 3,
    -- foldr is like for or reduce() backwards in JS, fold + r, r stands for right, so it iterate from right to left
    -- card is the current card, in this sequence 3 -> 2 -> 1
    -- acc in my case stands from accumulator, it basically appends to it each letter and a space, 
    -- \ -> is a lambda function, which has 2 arguments, in this case acc and card, and returns card ++ " " ++ acc, in other words 3 2 1
    -- The "" after the () refers to the inital value of the accumulator, in this case I want it to be a string to append to it and have spaces
    -- playerDeck is the array I'm looping over                   
    -- keep in mind card / acc are just variable names
    putStr (playerConcat ++ " " ++ "(Total: " ++ show(calculateDeck playerDeck) ++ ")") -- print the playerContac which is a string

    if showRobot then do -- now we want to check who's cards we are displaying, if is the user we don't have any restrictions, if it's the dealer we have to hide the first element, in this case is a ?
        putStr "\n| Dealer's deck: " -- this is used when the game ends, we want to show all the cards 
        let dealerConcat = foldr (\card acc -> card ++ " " ++ acc) "" dealerDeck -- same as before
        putStr dealerConcat
    else do
        putStr "\n| Dealer's deck: ? " -- display when the game is playing
        let dealerConcat = foldr (\card acc -> card ++ " " ++ acc) "" (tail dealerDeck) -- tail -> primeste o lista si returneaza lista fara primul element, ( exact ce ne trebuie pe scurt)
        putStr (dealerConcat ++ "(Total: " ++ show(calculateDeck (tail dealerDeck)) ++ ")") -- UI impovements, automatically calculates the deck

    putStr "\n+-----------------------+"
    putStr "\n>>> [ENTER] Hit | [TYPE ANYTHING] Stay\n" -- UI&UX

calculateDeck :: [String] -> Int 
calculateDeck deck = sum (map cardValueToINT deck) -- we calculate de deck value using sum (), we give sum the arguments of (map cardValueToInt deck) which means it returns an array of numbers,

getGameStatus :: [String] -> [String] -> Bool -> String 
getGameStatus playerDeck dealerDeck isStay
    | isStay && playerSum > dealerSum = "Player wins" -- isStay means if the player doesn't want any more cards
    | isStay && playerSum < dealerSum = "Dealer wins" -- this conditions has to be done first
    | isStay && playerSum == dealerSum = "DRAW"

    | playerSum > 21 && dealerSum > 21 = "DRAW"
    | playerSum >= 21 = "Dealer wins"
    | dealerSum >= 21 = "Player wins"
    | playerSum == 21 = "Player wins with 21"
    | dealerSum == 21 = "Dealer wins with 21"
    | otherwise = "Game continues"
    where 
        playerSum = calculateDeck playerDeck -- here we define the variables,
        dealerSum = calculateDeck dealerDeck

whileLoop :: [String] -> [String] -> [String] -> Bool -> IO() -- here is where i have the game running, using a recursive function, like while
whileLoop cards playerCards dealerCards gameOver
    | gameOver = return () -- if the game is over, retrun so basically exit
    | otherwise = do 
            input <- getLine -- reads from the user

            if input == "" then do -- if the user presses enter
                playerNewCard <- getCard cards True (calculateDeck playerCards) -- get a new card with from getCard fnc
                --True stands for, isUser, then gives the sumDeck
                let newPlayerCards = playerCards ++ [playerNewCard]
                -- In haskell we cannot edit already implemented variables, so i need to create another one
                -- newPLayerCards is the deck with the new card added

                -- we need to create a new variable to store the new cards
                let new_cards = removeFirstCard cards

                dealerNewCard <- getCard new_cards False  (calculateDeck dealerCards) -- same as users
                let newRobotCards = dealerCards ++ [dealerNewCard]

                let gameStatus = getGameStatus newPlayerCards newRobotCards False
                -- in gameStatus we will save a string that tell s the game status

                -- we cannot use new_cards again, because in haskeel you cannot reasign, so i've changed the name
                let new_cards_2 = removeFirstCard new_cards
                if gameStatus == "Game continues" then do -- if no one wins or lose continue the game
                    displayDecks newPlayerCards newRobotCards False -- display the cards of both of them, with the false argument meaning not show the dealer hand
                    whileLoop new_cards_2 newPlayerCards newRobotCards False
                else do
                    displayDecks newPlayerCards newRobotCards True -- if the game stops we can show the dealer hand
                    fancyPrintGameStatus gameStatus;
            else do
                let dealerDeckVal = calculateDeck dealerCards
                if dealerDeckVal < 17 then do -- as the rule says, if you stay and dealer's hand is lower than 17, he has to get a card
                    newCard <- getCard cards False dealerDeckVal -- get a new card
                    putStrLn ("Dealers hand is lower than 17, so he takes a card..." ++ ("The Dealer got a " ++ newCard))
                    displayDecks playerCards (dealerCards ++ [newCard]) True -- calculate the deck with the new card added
                    fancyPrintGameStatus (getGameStatus playerCards dealerCards True)
                else do -- otherwise keep it normal
                    displayDecks playerCards dealerCards True -- if the user wants to stay, we show all cards and print the winner / loser
                    fancyPrintGameStatus (getGameStatus playerCards dealerCards True)
                
startGame :: IO () -- this is the "main" function, the function that starts the game
startGame = do
    -- we use cards that are shuffled
    cards :: [String] <- shuffleCards all_cards

    playerCards <- getInitialDeck cards True 0 -- we initialize the user cards and dealers
    let new_cards = removeFirstCard cards -- store new cards

    dealerCards <- getInitialDeck new_cards False 1001 -- 
    let new_cards_2 = removeFirstCard new_cards 

    displayDecks playerCards dealerCards False -- initally display the decks
    whileLoop new_cards_2 playerCards dealerCards False -- then proceed to play the game




