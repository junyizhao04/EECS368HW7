import Data.Char

--start a board with no input, saved by a int list of how many stars in each row
initBoard :: [Int]
initBoard = [5,4,3,2,1]

--see who's the next player. since it's internal, there is no foolproof features. Input: player's number, output: next player's number
nextPlayer :: Int -> Int
nextPlayer 1 = 2
nextPlayer 2 = 1

--input digit. Use getline to enable backspace and additional input for foolproof, since we only take the first char (different from reference). Input: message to display, output: the digit we fetched
getDigit :: String -> IO Int
getDigit message = do putStr message --should be enter row/stars to remove
                      putChar '\n'
                      x <- getLine -- enable players to think before they decide. Use "Enter" to decide.
                      if isDigit (x !! 0) then
                        do return (digitToInt (x !! 0)) -- only return if first char is a valid int
                      else 
                        do putStrLn "Invalid Input! Try again" --or display error message, iterate endlessly until right input
                           getDigit message

--print the board by stars, a front end work. Input: the board array, output: a bunch of stars on terminal.
printBoard :: [Int] -> IO ()
printBoard board = putStr $ unlines [(show row) ++ ": " ++ replicate stars '*'| (stars, row) <- zip board [1..length board]]

--see if the board is cleared up, then it means the game ends. Input: the board, output: bool seeing if game ended
checkEnd :: [Int] -> Bool
checkEnd board = all (==0) board

--as always, compliance checker. See if player input valid on taking out some stars on a row (cannot be empty or nonexistent row or more stars than what we have). Input: action to do, output: compliant or not
checkCompliance :: [Int] -> Int -> Int -> Bool
checkCompliance board row rStars = if(row <= 5) && (row > 0) then --foolproof function to stop player from triggering out-of-index row
                                     if (board !! (row - 1) >= rStars) then True else False --stop player from deleting more stars than we have in a row
                                   else False  

--when compliant, delete the stars and generate new play board. Input: compliant action to do, output: new board
generateNewBoard :: [Int] -> Int -> Int -> [Int]
generateNewBoard board row rStars = [if irow == row then stars - rStars else stars | (stars, irow) <- zip board [1..length board]]

--play file. Input: board and player number, output: game on terminal
playnim :: [Int] -> Int -> IO ()
playnim board player = do printBoard board -- print the board
                          if checkEnd board then -- see if board is empty first
                            do putStr "Player "
                               putStr (show (nextPlayer player)) -- if already empty, then last player cleared it up and they win
                               putStrLn " wins!"
                          else -- if not, continue the game
                            do putStr "Player "
                               putStrLn (show player) -- print player's number
                               row <- getDigit "Enter row number: " -- enter row
                               stars <- getDigit "Enter stars to remove: " -- enter stars
                               if checkCompliance board row stars then --check compliance
                                 do  putStrLn "Input validated, continue to next round!" --if compliant, do the action and proceed to next round
                                     playnim (generateNewBoard board row stars) (nextPlayer player) -- use playnim to start next round
                               else
                                 do putStrLn "This move is invalid, try again!" -- if not compliant, show error message, iterate endlessly until right input
                                    playnim board player

main :: IO ()
main = playnim initBoard 1 --start with new board and player 1
