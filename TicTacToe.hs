-- Julia Żur & Kuba Gwóźdź

import Data.List
import Data.Char


clear = putStr "\ESC[2J"

type Table = [[Char]]

type Move = (Int, Int)

emptyTable3 = ["   ", "   ", "   "]

play = do
    clear
    putStrLn "\nTicTacToe!\n"
    putStrLn "To start game enter move LN, where L is Letter of column and N is number of row"
    game emptyTable3 'X'

game :: Table -> Char -> IO()
game table playerIcon = do
    putStr $ showTable $ table
    putStrLn ("Player's " ++ [playerIcon] ++ " turn"++"\n"++"Enter your move: ")
    line <- getLine
    clear
    if line == "quit" then return()
    else do
        let move = parseMove line
        if (valid move table) then do
            let newTable = putMove move table playerIcon
            if winner newTable then do
                putStr $ showTable $ newTable
                putStrLn ("Player "++[playerIcon]++" won!")
                playAgain
            else if (endOfGame newTable) then do
                putStrLn "Tie!"
                playAgain
            else do
                game newTable (nextPlayer playerIcon)
        else do
           putStrLn "Wrong move"
           game table playerIcon


playAgain :: IO()
playAgain = do
    putStrLn "Play again ? (Y/N) "
    rematch <- getLine
    if rematch == "Y" then do
        clear
        play
    else if rematch == "N" then do
        putStrLn "Thank you for game :)"
        return()
    else do
        putStrLn "Wrong input. Enter Y or N"
        playAgain



showTable :: Table -> String
showTable table = 
    letters ++  "\n" ++ (intercalate separator $ lineNumber $ map row table)++"\n"
    where
        letters      = "    A   B   C   "
        separator    = "\n"++ "   ---+---+--- " ++ " \n"
        lineNumber s = [(show n) ++ "   " ++ x | n <- [0..2], x <- [s!!n]] 
        row          = intercalate " | " . map (\x -> [x])


parseMove :: String -> Move
parseMove line 
    | length (line) /=2 = (-1,-1)
    | (elem l ['A'..'Z']) && (elem n ['0'..'9']) = ( ((ord l)-65, (ord n)-48))
    | otherwise = (-1,-1)                         -- Wrong move
    where   -- B3
        l = head line
        n = head . tail $ line

valid :: Move -> Table -> Bool
valid (x,y) table
    | x < 0 || y < 0 = False
    | x>2 || y>2 = False
    | isOccupied (x,y) table = False
    | otherwise = True

isOccupied :: Move -> Table -> Bool
isOccupied (x,y) table = (table!!y)!!x /= ' '


putMove :: Move -> Table -> Char -> Table
putMove (x,y) (a:b:c) playerIcon | y == 0 = (replaceNth x playerIcon a):b:c
                                 | y == 1 = a:(replaceNth x playerIcon b):c
                                 | otherwise = a:b:[(replaceNth x playerIcon (head c))]



replaceNth :: Int -> Char -> [Char] -> [Char]
replaceNth n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs


allSame :: [Char] -> Bool
allSame (x:xs) = all (==x) xs && (x/=' ')

winner :: Table -> Bool
winner table = vertical || horizontal || dUpLeft || dUpRight
    where
        horizontal = (allSame (table !! 0) )  || ( allSame (table !! 1))  || ( allSame (table !! 2 ))
        vertical   = allSame (column 0 table) || allSame (column 1 table) || allSame (column 2 table)
        dUpLeft    = allSame (upLeft table)   && (not $ all (== ' ') (upLeft table)) 
        dUpRight   = allSame (upRight table)  && (not $ all (== ' ') (upRight table)) 

column :: Int -> Table -> [Char]
column n (x:xs:ys) = x!!n : xs!!n : (head ys)!!n:[]

upLeft :: Table -> [Char]
upLeft (x:xs:ys) = x!!0 : xs!!1 : (head ys) !! 2:[]

upRight :: Table -> [Char]
upRight (x:xs:ys) = x!!2 : xs!!1 : (head ys)!!0:[]

endOfGame :: Table -> Bool
endOfGame (x:xs:ys:_) = all (/=' ') (x++xs++ys)

nextPlayer :: Char -> Char
nextPlayer curr = if curr == 'X' then 'O' else 'X'
