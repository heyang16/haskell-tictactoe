module TicTacToe where

import Data.Char
import Data.Maybe
import Data.List
import Text.Read

-------------------------------------------------------------------
data Player = O | X
            deriving (Eq, Show)

data Cell = Empty | Taken Player
          deriving (Eq, Show)

type Board = ([Cell], Int)

type Position = (Int, Int)

-------------------------------------------------------------------

--
-- Some useful functions from, or based on, the unassessed problem sheets...
--

-- Preserves Just x iff x satisfies the given predicate. In all other cases
-- (including Nothing) it returns Nothing.
filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe p m@(Just x)
  | p x = m
filterMaybe p _
  = Nothing

-- Replace nth element of a list with a given item.
replace :: Int -> a -> [a] -> [a]
replace 0 p (c : cs)
  = p : cs
replace _ p []
  = []
replace n p (c : cs)
  = c : replace (n - 1) p cs

-- Returns the rows of a given board.
rows :: Board -> [[Cell]]
rows (cs , n)
  = rows' cs
  where
    rows' []
      = []
    rows' cs
      = r : rows' rs
      where
        (r, rs) = splitAt n cs

-- Returns the columns of a given board.
cols :: Board -> [[Cell]]
cols
  = transpose . rows

-- Returns the diagonals of a given board.
diags :: Board -> [[Cell]]
diags (cs, n)
  = map (map (cs !!)) [[k * (n + 1) | k <- [0 .. n - 1]],
                      [k * (n - 1) | k <- [1 .. n]]]

-------------------------------------------------------------------
-- Checks if the given board represents a winning situation
gameOver :: Board -> Bool
gameOver b = or [x `elem` lines | x <- [[Taken O], [Taken X]]]
  where
    lines = map nub $ rows b ++ cols b ++ diags b

-- Checks if the given board represents a drawn situation
gameDrawn :: Board -> Bool
gameDrawn b = and [Empty `notElem` r | r <- rows b]

-------------------------------------------------------------------

-- Given an input string determines whether it can be parsed as two integers
-- Moves must be of the form "row col" where row and col are integers
-- separated by whitespace. Bounds checking happens in tryMove, not here.
-- If unsuccesful, return Nothing
parsePosition :: String -> Maybe Position
parsePosition inp = case inp' of
  [Just x, Just y] -> Just (x, y) -- Checks if list has only 2 "just" elements
  _ -> Nothing
  where
    inp' = map readMaybe (words inp) :: [Maybe Int]

-- Given a player and their move and the current board, tries to execute a move
-- returns the new board if successful and Nothing if unsuccessful
tryMove :: Player -> Position -> Board -> Maybe Board
tryMove pl (x, y) (b, n)
  -- Checks if index is in range
  | x < 0 || y < 0 = Nothing
  | x >= n || y >= n = Nothing
  -- Checks if cell is empty
  | b !! (n * y + x) /= Empty = Nothing
  -- replaces cell
  | otherwise = Just (b', n)
    where
      b' = replace (n * y + x) (Taken pl) b

-------------------------------------------------------------------
-- I/O Functions

-- Prints the game board
prettyPrint :: Board -> IO ()
prettyPrint board@(b, n) = do
  let r = rows board
  putStrLn $ replicate (n * 4 - 3) '_'
  mapM_ printLine [intersperse ' ' $ intersperse '|' (map cellToChar row) | row <- r]
  where
    cellToChar :: Cell -> Char -- Converts a cell to the char representing it
    cellToChar (Taken O) = 'O'
    cellToChar (Taken X) = 'X'
    cellToChar Empty = '-'

    printLine :: String -> IO()
    printLine s = do 
      putStrLn s
      putStrLn $ replicate (n * 4 - 3) '_'

-- The following reflect the suggested structure, but you can manage the game
-- in any way you see fit.

-- Repeatedly read a target board position and invoke tryMove until
-- the move is successful (Just ...).
takeTurn :: Board -> Player -> IO Board
takeTurn b pl = do
  putStrLn ("\nPlayer " ++ show pl ++ ", please make a move.")
  putStrLn "Please enter a position to play in the form: x y"
  inp <- getLine
  case parsePosition inp of -- Checks if input can be parsed into coordinates
    -- Checks if coordinates are playable
    Just pos -> case tryMove pl pos b of
      Just b' -> return b'
      Nothing -> do
        putStrLn "\nInvalid coordinates."
        takeTurn b pl
    Nothing -> do
      putStrLn "\nInvalid input."
      takeTurn b pl

-- Manage a game by repeatedly: 1. printing the current board, 2. using
-- takeTurn to return a modified board, 3. checking if the game is over,
-- printing the board and a suitable congratulatory message to the winner
-- if so.
playGame :: Board -> Player -> IO ()
playGame b pl = do
  prettyPrint b -- Prints the current board
  b' <- takeTurn b pl -- Plays a turn and gets the new board
  if gameOver b' then do 
    prettyPrint b' -- Prints the ending board
    putStrLn ("\nCongratulations, the winner is " ++ show pl ++ "!")
  else if gameDrawn b' then do
    prettyPrint b'
    putStrLn "\nGame drawn!"
  else do
    playGame b' pl' -- Continues the game with new board and next player
      where
        pl' = head $ filter (/= pl) [O, X]

-- Make a new board by requesting a board size
initBoard :: IO Board
initBoard = do
  putStrLn "\nBegin by entering the size of your board:"
  md <- getLine
  case readMaybe md :: Maybe Int of 
    Just d -> do
      putStrLn $ "\nBoard size set to " ++ show d
      return (b, d)
        where b = replicate (d * d) Empty
    Nothing -> do
      putStrLn "\nInvalid board size."
      initBoard

-- Checks if user wants to play again
playAgain :: IO Bool
playAgain = do
  putStrLn "\nPlay again? [y/n]"
  a <- getLine
  if a `elem` ["y", "n"] then
    return $ a == "y"
  else do
    putStrLn "\nInvalid input."
    playAgain



-- Print a welcome message, read the board dimension, invoke playGame and
-- exit with a suitable message.
main :: IO ()
main = do
  putStrLn "\nWelcome to TicTacToe!"
  d <- initBoard
  playGame d X
  a <- playAgain
  if a then main else do
    putStrLn "\nThank you for playing!"  
    return ()

  
  

-------------------------------------------------------------------

testBoard1, testBoard2, testBoard3 :: Board

testBoard1
  = ([Taken O,Taken X,Empty,Taken O,
      Taken O,Empty,Taken X,Taken X,
      Taken O,Empty,Empty,Taken X,
      Taken O,Taken X,Empty,Empty],
      4)

testBoard2
  = ([Taken X,Empty,
      Empty,Empty],
      2)

testBoard3
  = ([Taken O,Taken X,Empty,Taken O,Taken X,
      Taken O,Empty,Taken X,Taken X,Empty,
      Empty,Empty,Taken X,Taken O,Taken O,
      Taken O,Taken X,Empty,Empty,Taken X,
      Taken X,Empty,Taken O,Empty,Empty],
      5)
