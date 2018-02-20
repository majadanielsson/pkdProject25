-- Project group 25; Minesweeper
-- Erik Hellström, Maja Danielsson, Martin Dannelind

import System.Random
import System.IO
import Data.List
import Control.Monad
import System.Exit

-----------------------------------------------------------------------------------------------------------------------------

{- Cell is what each coordinate in the game is. It is either a Unexplored cell, a mine or a clue. 
  INVARIANT: ????????????????
-}
data Cell = Unexplored | Mine | Clue Int deriving (Show, Eq)

-- Coords tells the position of each cell in the game.
type Coords = (Int, Int)
-- Board is the game field that is made up by a list of tuples containing a cell with its coordinate and data Cell value.
type Board = [(Coords, Cell)]
-- MineTracker is a list of coordinates for the location of the mines on the board.
type MineTracker = [(Int, Int)]

-----------------------------------------------------------------------------------------------------------------------------

{- main
   A brief human-readable description of the purpose of the function.
   PRE:  ... pre-condition on the arguments, if any ...
   RETURNS: ... description of the result, in terms of the arguments ...
   SIDE EFFECTS: ... side effects, if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
main :: IO ()
main = do
  putStrLn "Let's play MineSweeper"
  gamePlay unexploredBoard facit
  putStrLn "Game Over"

{- gamePlay
   A brief human-readable description of the purpose of the function.
   PRE:  ... pre-condition on the arguments, if any ...
   RETURNS: ... description of the result, in terms of the arguments ...
   SIDE EFFECTS: ... side effects, if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
gamePlay :: Board -> Board -> IO ()
gamePlay gameBoard solution
  | filter (\((x,y), c) -> c /= Mine) solution == filter (\((x,y), c) -> c /= Unexplored) gameBoard = do
    putStrLn "YOU WON!"
    putStrLn ""
    playAgain
  | gameBoard == solution = do
    printBoard solution
    putStrLn ""
    putStrLn "You a loser LOL XD"
    putStrLn ""
    playAgain
  | otherwise = do
    printBoard gameBoard
    putStrLn "Which cell do you want to reveal?"
    putStrLn "Letter? (lower case)"
    y <- getLine
    putStrLn "Number?"
    x <- getLine
    gamePlay (gamePlayAux gameBoard solution (read x) (charToInt y)) solution

{- gamePlayAux b1 b2 x y
   Reveals what is hidden at a given coordinate on the board
   PRE: True
   RETURNS: b1 with the coordinate (x,y) changed based on b2
   SIDE EFFECTS: ... side effects, if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
gamePlayAux :: Board -> Board -> Int -> Int -> Board
gamePlayAux gameBoard solution x y
                     | 19 < x || x < 0 || 19 < y || y < 0 = gameBoard
                     -- | snd (head (gamePlayAux' (x,y) solution)) == Clue 0 =
                     | snd (head (gamePlayAux' (x,y) solution)) == Mine = solution
                     | otherwise = insertCells (gamePlayAux' (x,y) solution) gameBoard
                        where
                          gamePlayAux' (x,y) [] = [((x,y), Unexplored)]
                          gamePlayAux' (x,y) (((a,b), c):as)
                              | (x,y) == (a,b) = [((x,y), c)]
                              | otherwise = gamePlayAux' (x,y) as
{- playAgain
   A brief human-readable description of the purpose of the function.
   PRE:  ... pre-condition on the arguments, if any ...
   RETURNS: ... description of the result, in terms of the arguments ...
   SIDE EFFECTS: ... side effects, if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
playAgain :: IO ()
playAgain = do
  putStrLn ""
  putStrLn "Want to play again? y or n"
  putStrLn ""
  ans <- getLine
  if ans == "y"
  then do main
  else do exitFailure

{- charToInt s
   Maps each letter on the board to its corresponding integer coordinate
   PRE:  ... pre-condition on the arguments, if any ...
   RETURNS: ... description of the result, in terms of the arguments ...
   SIDE EFFECTS: ... side effects, if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
charToInt :: String -> Int
charToInt str
  |str=="a"=0|str=="b"=1|str=="c"=2|str=="d"=3|str=="e"=4|str=="f"=5|str=="g"=6|str=="h"=7
  |str=="i"=8|str=="j"=9|str=="k"=10|str=="l"=11|str=="m"=12|str=="n"=13|str=="o"=14|str=="p"=15
  |str=="q"=16|str=="r"=17|str=="s"=18|str=="t"=19| otherwise = 21

{- printBoard b
   Prints the board in the terminal with underlaying letters and numbers on the right side to help the user to point
   out the right coordinate.
   PRE:  ... pre-condition on the arguments, if any ...
   RETURNS: ... description of the result, in terms of the arguments ...
   SIDE EFFECTS: ... side effects, if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
printBoard :: Board -> IO ()
printBoard playerBoard = printBoard' ((divideBoardString (stringBoard playerBoard) 0)
                         ++ ["____________________________________________________________"]
                         ++ [" a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t "])
                where
                  printBoard' (x:xs) = mapM_ print (x:xs)

{- stringBoard b
   Converts all cells in a board into how they are to be displayed on the game board
   PRE:  ... pre-condition on the arguments, if any ...
   RETURNS: b where all elements are converted from type Board into how they are to
            be displayed on the game board
   SIDE EFFECTS: ... side effects, if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
stringBoard :: Board -> String
stringBoard [] = []
stringBoard (x:xs) = showCell (snd x) ++ stringBoard xs

{- divideBoardString s acc
   Divides a string into groups of 60 and ends each new string with a boarder what number
   string it is, starting at 0
   PRE: True
   RETURNS: s divided into acc+1 groups of 60
   SIDE EFFECTS: ... side effects, if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
divideBoardString :: String -> Int -> [String]
divideBoardString [] b = []
divideBoardString xs b = [(take 60 xs) ++ "|" ++ (show b)] ++ divideBoardString (drop 60 xs) (b+1)

{- showCell cell
   Converts a cell into how it is to be displayed on the game board
   PRE:  ... pre-condition on the arguments, if any ...
   RETURNS: ... description of the result, in terms of the arguments ...
   SIDE EFFECTS: ... side effects, if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
showCell :: Cell -> String
showCell (Clue 0)   = "   "
showCell (Clue n)   = " " ++ (show n) ++ " "
showCell Unexplored = " = "
showCell Mine       = " x "

{- emptyBoard 
   Creates a board consisting of only empty cells (Clue 0)
   PRE:  ... pre-condition on the arguments, if any ...
   RETURNS: ... description of the result, in terms of the arguments ...
   SIDE EFFECTS: ... side effects, if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
emptyBoard :: Board
emptyBoard = emptyBoard' 0 0
              where
                emptyBoard' :: Int -> Int -> Board
                -- VARIANT: value x, value y
                emptyBoard' 20 y = []
                emptyBoard' x 19 = [((x,19), Clue 0)] ++ emptyBoard' (x+1) 0
                emptyBoard' x y = [((x,y), Clue 0)] ++ emptyBoard' x (y+1)

{- unexploredBoard 
   Creates a board consisting of only unexplored cells (Unexplored)
   PRE:  ... pre-condition on the arguments, if any ...
   RETURNS: ... description of the result, in terms of the arguments ...
   SIDE EFFECTS: ... side effects, if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
unexploredBoard :: Board
unexploredBoard = unexploredBoard' 0 0
              where
                unexploredBoard' :: Int -> Int -> Board
                --VARIANT: value x, value y 
                unexploredBoard' 20 y = []
                unexploredBoard' x 19 = [((x,19), Unexplored)] ++ unexploredBoard' (x+1) 0
                unexploredBoard' x y = [((x,y), Unexplored)] ++ unexploredBoard' x (y+1)

{- randomMineCells
   Creates a board of 14 (not so random) mines...
   PRE:  ... pre-condition on the arguments, if any ...
   RETURNS: ... description of the result, in terms of the arguments ...
   SIDE EFFECTS: ... side effects, if any, including exceptions ...
   EXAMPLES: randomMineCells = [((0,2), Mine), ((4,6),Mine),((4,5), Mine),((5,7), Mine)]
-}
randomMineCells :: Board
randomMineCells = [((0,2), Mine), ((4,6),Mine),((4,5), Mine),((5,7), Mine),((1,6), Mine),((8,2), Mine),
                   ((9,9), Mine), ((11,12), Mine), ((14,16),Mine),((14,15), Mine),((15,17), Mine),
                   ((11,16), Mine),((18,12), Mine),((19,19), Mine)]

{- insertCells b newB
   A brief human-readable description of the purpose of the function.
   PRE:  ... pre-condition on the arguments, if any ...
   RETURNS: ... description of the result, in terms of the arguments ...
   SIDE EFFECTS: ... side effects, if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
insertCells :: Board -> Board -> Board
insertCells [] playboard = playboard
insertCells (x:xs) playboard = insertCells xs (insertCells' x playboard)
                     where
                      insertCells' ((x,y), c1) (((a,b), c2):as)
                          | (x,y) == (a,b) = (((a,b), c1):as)
                          | otherwise = ((a,b), c2): insertCells' ((x,y), c1) as

{- trackMines b
   Computes the coordinates adjacent to the coordinates of the cells on the board
   PRE:  ... pre-condition on the arguments, if any ...
   RETURNS: ... description of the result, in terms of the arguments ...
   SIDE EFFECTS: ... side effects, if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
trackMines :: Board -> MineTracker
trackMines playboard =  filter (\(x,y) -> x /= -1 && x /= 20 && y /= -1 && y /= 20) (trackMines' playboard)
                where
                 trackMines' [] = []
                 trackMines' (((x,y), _ ):xs) =
                     trackMines' xs ++ [(x+1, y-1), (x, y-1), (x-1, y-1),(x+1, y),
                                       (x-1, y), (x+1, y+1), (x, y+1), (x-1, y+1)]

{- insertTrack mineT b
   A brief human-readable description of the purpose of the function.
   PRE:  ... pre-condition on the arguments, if any ...
   RETURNS: ... description of the result, in terms of the arguments ...
   SIDE EFFECTS: ... side effects, if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
insertTrack :: MineTracker -> Board -> Board
insertTrack [] playboard = playboard
insertTrack ((x,y):xs) playboard = insertTrack xs (insertTrack' (x,y) playboard)
                    where
                      insertTrack' (x,y) (((a,b), Mine):as)
                            | (x,y) == (a,b) = (((a,b), Mine):as)
                            | otherwise = ((a,b), Mine): insertTrack' (x,y) as
                      insertTrack' (x,y) (((a,b), Clue c):as)
                            | (x,y) == (a,b) = (((a,b), Clue (c+1)):as)
                            | otherwise = ((a,b), Clue c): insertTrack' (x,y) as

facit = (insertTrack (trackMines randomMineCells) (insertCells randomMineCells emptyBoard))
