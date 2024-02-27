module Main (main) where

import Game
import Minimax (playBest)
import System.IO (hFlush, stdout)

-- Function to display the current state of the game board
displayBoard :: BigBoard -> IO ()
displayBoard bigBoard = putStrLn $ serializeBigBoard bigBoard

-- Function to serialize the BigBoard for display
serializeBigBoard :: BigBoard -> String
serializeBigBoard bigBoard = concatMap (\sq -> serializeSquare sq ++ "\n") bigBoard
  where
    serializeSquare :: Square -> String
    serializeSquare (Win player) = show player ++ " wins this board\n"
    serializeSquare (Board board) = serializeBoard board ++ "\n"

-- Helper to serialize a single board
serializeBoard :: Board -> String
serializeBoard board = unlines [unwords [serializePlayer (board !! (r * 3 + c)) | c <- [0..2]] | r <- [0..2]]
  where
    serializePlayer :: Maybe Player -> String
    serializePlayer Nothing = "_"
    serializePlayer (Just X) = "X"
    serializePlayer (Just O) = "O"

-- Main game loop
gameLoop :: State -> IO ()
gameLoop state@(State bigBoard player _) = do
  displayBoard bigBoard
  if player == O then do
    putStrLn "Computer's turn..."
    let action = playBest state
    let newState = case play state action of
                    Continue s -> s
                    End (Just winner) _ -> State bigBoard winner (-1)
                    End Nothing _ -> state
    gameLoop newState
  else do
    putStrLn "Your turn. Enter your move as 'boardPosition squarePosition':"
    move <- getLine
    let action = readAction move
    case action of
      Just a -> case play state a of
                  Continue s -> gameLoop s
                  End (Just winner) _ -> putStrLn $ show winner ++ " wins!"
                  End Nothing _ -> putStrLn "It's a draw!"
      Nothing -> do
        putStrLn "Invalid move. Try again."
        gameLoop state

-- Function to read an action from the user's input
readAction :: String -> Maybe Action
readAction str =
  case words str of
    [boardStr, posStr] -> Just (Action (read boardStr) (read posStr))
    _ -> Nothing

main :: IO ()
main = do
  putStrLn "Starting Tic-Tac-Toe..."
  let initialState = Game.initialState
  gameLoop initialState
