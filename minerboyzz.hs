-- Project group 25; Minesweeper
-- Erik Hellström, Maja Danielsson, Martin Dannelind

import System.IO
import Control.Monad
import System.Exit
import Test.HUnit

-----------------------------------------------------------------------------------------------------------------------------

{- Cell is what each coordinate in the game is. It is either a Unexplored cell, a mine or a clue.
  INVARIANT: ????????????????
-}
data Cell = Unexplored | Mine | Clue Int deriving (Show, Eq)


-- Coords tells the position of each cell on the board.
-- INVARIANT: Int > 0 in (Int,Int)
type Coords = (Int, Int)
-- Board is the game field that is made up by a list of tuples containing a cell with its coordinate and data Cell value.
type Board = [(Coords, Cell)]

-----------------------------------------------------------------------------------------------------------------------------

{- main
   Starts the game.
   PRE: True
   RETURNS: ... description of the result, in terms of the arguments ...
   SIDE EFFECTS: ... side effects, if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
main :: IO ()
main = do
  putStrLn ""
  putStrLn "Let's play MineSweeper!"
  putStrLn "What level would you like to play? easy, medium or hard"
  ans <- getLine
  if ans == "easy"
    then do
  gamePlay unexploredBoard (boardGenerator easyBoard)
  else if ans == "medium"
    then do
  gamePlay unexploredBoard (boardGenerator mediumBoard)
  else if ans == "hard"
    then do
  gamePlay unexploredBoard (boardGenerator difficultBoard)
  else main
  putStrLn "Game Over"

{- gamePlay
   Asks the user wich cell to uncover and later tells the user if they have won or lost.
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
    putStrLn "You lost..."
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
   Reveals what is hidden at a given coordinate on the board.
   PRE: True
   RETURNS: b1 with the coordinate (x,y) changed based on b2
   SIDE EFFECTS: ... side effects, if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
gamePlayAux :: Board -> Board -> Int -> Int -> Board
gamePlayAux gameBoard solution x y
        | 19 < x || x < 0 || 19 < y || y < 0 = gameBoard
        | snd (head (findCell (x,y) solution)) == Clue 0 =
            insertCells ((expand [((x,y), Clue 0)] solution [((x,y), Clue 0)]) ++ [((x,y), Clue 0)]) gameBoard
        | snd (head (findCell (x,y) solution)) == Mine = solution
        | otherwise = insertCells (findCell (x,y) solution) gameBoard

playAgain = do
  putStrLn ""
  putStrLn "Would you like to play again? y or n"
  putStrLn ""
  ans <- getLine
  if ans == "y"
  then do main
  else do exitFailure
{- expand b1 b2
   A brief human-readable description of the purpose of the function.
   PRE:  ... pre-condition on the arguments, if any ...
   RETURNS: ... description of the result, in terms of the arguments ...
   SIDE EFFECTS: ... side effects, if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
expand :: Board -> Board -> Board -> Board
expand cells solution cellCount
      | checkZero cells == False = cells ++ cellCount
      | otherwise =
        expand (filter (`notElem` cellCount) (expand' (trackNeighb ((filter ((== Clue 0).snd) cells))) solution)) solution (cells++cellCount)
--where
expand' [] _ = []
expand' (x:xs) solution = (findCell x solution) ++ expand' xs solution

checkZero :: Board -> Bool
checkZero [] = False
checkZero (((x,y), c):xs)
               | c == Clue 0 = True
               | otherwise = checkZero xs

{- findCell c b
   A brief human-readable description of the purpose of the function.
   PRE:  ... pre-condition on the arguments, if any ...
   RETURNS: ... description of the result, in terms of the arguments ...
   SIDE EFFECTS: ... side effects, if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
findCell :: Coords -> Board -> Board
findCell (x,y) (((a,b), c):as)
          | (x,y) == (a,b) = [((x,y), c)]
          | otherwise = findCell (x,y) as

{- charToInt s
   Maps each letter on the board to its corresponding integer coordinate.
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
   out the right coordinate and cell they want to unvail.
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
   Converts all cells in a board into how they are to be displayed on the game board.
   PRE:  ... pre-condition on the arguments, if any ...
   RETURNS: b where all elements are converted from type Board into how they are to
            be displayed on the game board
   SIDE EFFECTS: ... side effects, if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
stringBoard :: Board -> String
-- VARIANT: length (x:xs)
stringBoard [] = []
stringBoard (x:xs) = showCell (snd x) ++ stringBoard xs

{- divideBoardString s acc
   Divides a string into groups of 60 and ends each new string with a boarder what number
   string it is, starting at 0.
   PRE: True
   RETURNS: s divided into acc+1 groups of 60
   SIDE EFFECTS: ... side effects, if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
divideBoardString :: String -> Int -> [String]
-- VARIANT: length xs
divideBoardString [] b = []
divideBoardString xs b = [(take 60 xs) ++ "|" ++ (show b)] ++ divideBoardString (drop 60 xs) (b+1)

{- showCell cell
   Converts a cell into how it is to be displayed on the game board.
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

boardGenerator mines = insertMines (trackNeighb mines) (insertCells mines emptyBoard)

{- emptyBoard
   Creates a board consisting of only empty cells (Clue 0).
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
   Creates a board consisting of only unexplored cells (Unexplored).
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

{- insertCells b1 b2
   A brief human-readable description of the purpose of the function.
   PRE:  ... pre-condition on the arguments, if any ...
   RETURNS: ... description of the result, in terms of the arguments ...
   SIDE EFFECTS: ... side effects, if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
insertCells :: Board -> Board -> Board
-- VARIANT: length (x:xs)
insertCells [] playboard = playboard
insertCells (x:xs) playboard = insertCells xs (insertCells' x playboard)
                    where
                      insertCells' :: (Coords, Cell) -> Board -> Board
                      -- INVARIANT: length xs
                      insertCells' ((x,y), c1) (((a,b), c2):as)
                          | (x,y) == (a,b)   = (((a,b), c1):as)
                          | otherwise        = ((a,b), c2): insertCells' ((x,y), c1) as


{- trackNeighb b
   Computes the coordinates adjacent to the coordinates of the cells on the board
   PRE:  ... pre-condition on the arguments, if any ...
   RETURNS: ... description of the result, in terms of the arguments ...
   SIDE EFFECTS: ... side effects, if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
trackNeighb :: Board -> [Coords]
trackNeighb playboard =  filter (\(x,y) -> x /= -1 && x /= 20 && y /= -1 && y /= 20) (trackNeighb' playboard)
                where
                 trackNeighb' [] = []
                 trackNeighb' (((x,y), _ ):xs) =
                     trackNeighb' xs ++ [(x+1, y-1), (x, y-1), (x-1, y-1),(x+1, y),
                                       (x-1, y), (x+1, y+1), (x, y+1), (x-1, y+1)]

{- randomMineCells
   Creates a board of 14 (not so random) mines...
   PRE:  ... pre-condition on the arguments, if any ...
   RETURNS: ... description of the result, in terms of the arguments ...
   SIDE EFFECTS: ... side effects, if any, including exceptions ...
   EXAMPLES: randomMineCells = [((0,2), Mine), ((4,6),Mine),((4,5), Mine),((5,7), Mine)]
-}

easyBoard :: Board
easyBoard = [((3, 1), Mine),((2, 8), Mine),((6, 3), Mine),((5, 6), Mine),((9, 4), Mine),((12, 6), Mine),
             ((13, 5), Mine),((14, 5), Mine),((17, 5), Mine),((19, 7), Mine),((16, 1), Mine),((15, 3), Mine),
             ((1, 11), Mine),((14, 12), Mine),((12, 14), Mine),((10, 14), Mine),((4, 9), Mine),((14, 9), Mine),
             ((7, 12), Mine),((5, 10), Mine),((7, 17), Mine),((9, 15), Mine),((4, 19), Mine),((2, 18), Mine),
             ((16, 14), Mine),((18, 14), Mine),((17, 16), Mine),((15, 17), Mine),((16, 19), Mine),((0,19), Mine)]

mediumBoard :: Board
mediumBoard = [((0,2), Mine), ((4,6),Mine),((4,5), Mine),((5,7), Mine),((1,6), Mine),((8,2), Mine),
                  ((9,9), Mine), ((11,12), Mine), ((14,16),Mine),((14,15), Mine),((15,17), Mine),
                  ((11,16), Mine),((18,12), Mine),((4,9), Mine),((4,11), Mine),((1,13), Mine),
                  ((15,3), Mine),((17,4), Mine),((13,13), Mine),((9,14), Mine),((3,15), Mine),
                  ((5,17), Mine),((2,19), Mine),((13,2), Mine),((13,6), Mine),((15,11), Mine),
                  ((7,6), Mine),((7,4), Mine),((10,1), Mine),((15,9), Mine),((8,17), Mine),
                  ((17,19), Mine),((19,5), Mine),((3,3), Mine),((18,13), Mine)]

difficultBoard :: Board
difficultBoard = [((9,9), Mine),((7,12), Mine),((13,14), Mine),((12,11), Mine),((6,14), Mine),((9,16), Mine),
                  ((15,9), Mine),((12,16), Mine),((17,10), Mine),((18,13), Mine),((16,15), Mine),((19,17), Mine),
                  ((17,19), Mine),((14,18), Mine),((9,18), Mine),((4,16), Mine),((2,18), Mine),((1,14), Mine),
                  ((3,12), Mine),((5,10), Mine),((5,7), Mine),((9,7), Mine),((7,5), Mine),((2,7), Mine),((0,9), Mine),
                  ((12,7), Mine),((17,5), Mine),((14,5), Mine),((19,6), Mine),((6,2), Mine),((15,0), Mine),
                  ((12,1), Mine),((11,3), Mine),((9,3), Mine),((6,3), Mine),((5,1), Mine),((2,3), Mine),
                  ((0,5), Mine),((19,2), Mine),((0,0), Mine)]
{- insertMines c b
   A brief human-readable description of the purpose of the function.
   PRE:  ... pre-condition on the arguments, if any ...
   RETURNS: ... description of the result, in terms of the arguments ...
   SIDE EFFECTS: ... side effects, if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
insertMines :: [Coords] -> Board -> Board
insertMines [] playboard = playboard
insertMines ((x,y):xs) playboard = insertMines xs (insertMines' (x,y) playboard)
                    where
                      insertMines' (x,y) (((a,b), Mine):as)
                            | (x,y) == (a,b) = (((a,b), Mine):as)
                            | otherwise = ((a,b), Mine): insertMines' (x,y) as
                      insertMines' (x,y) (((a,b), Clue c):as)
                            | (x,y) == (a,b) = (((a,b), Clue (c+1)):as)
                            | otherwise = ((a,b), Clue c): insertMines' (x,y) as

--[((2,0),Clue 0),((1,0),Clue 0),((0,0),Clue 0),((2,1),Clue 0), ((1,1), Mine), ((0,1),Clue 0),((2,2),Clue 0),((1,2),Clue 0),((0,2),Clue 0)]
{-test1 = TestCase ( assertEqual " Even 0" True ( even 0) )
test2 = TestCase ( assertEqual " Even 4" True ( even 4) )
test3 = TestCase ( assertEqual "Odd 4" False (odd 4) )
test4 = TestCase ( assertBool "Odd 3" (odd 3) )-}
test1 = TestCase (assertEqual "checkZero [((2,0),Clue 1),((1,0),Clue 0),((0,0),Clue 0),((2,1),Clue 1),((0,1),Clue 0),((2,2),Clue 1),((1,2),Clue 0),((0,2),Clue 0)]"
                  True (checkZero [((2,0),Clue 1),((1,0),Clue 0),((0,0),Clue 0),((2,1),Clue 1),((0,1),Clue 0),((2,2),Clue 1),((1,2),Clue 0),((0,2),Clue 0)]))
test2 = TestCase (assertEqual "insertMines [(2,0),(1,0),(0,0),(2,1),(0,1),(2,2),(1,2),(0,2)] [((2,0),Clue 0),((1,0),Clue 0),((0,0),Clue 0),((2,1),Clue 0),((1,1), Mine), ((0,1),Clue 0),((2,2),Clue 0),((1,2),Clue 0),((0,2),Clue 0)]"
                                [((2,0),Clue 1),((1,0),Clue 1),((0,0),Clue 1),((2,1),Clue 1),((1,1),Mine),((0,1),Clue 1),((2,2),Clue 1),((1,2),Clue 1),((0,2),Clue 1)]
                                (insertMines [(2,0),(1,0),(0,0),(2,1),(0,1),(2,2),(1,2),(0,2)] [((2,0),Clue 0),((1,0),Clue 0),((0,0),Clue 0),((2,1),Clue 0), ((1,1), Mine),
                                ((0,1),Clue 0),((2,2),Clue 0),((1,2),Clue 0),((0,2),Clue 0)]))
test3 = TestCase (assertEqual "trackNeighb [((3,7), Clue 0),((16,19),Clue 0)]"
                  [(17,18),(16,18),(15,18),(17,19),(15,19),(4,6),(3,6),(2,6),(4,7),(2,7),(4,8),(3,8),(2,8)]
                  (trackNeighb [((3,7), Clue 0),((16,19),Clue 0)]))


runTests = runTestTT (TestList [test1, test2, test3])
