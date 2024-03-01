module Main (main) where

-- Import necessary modules for game logic, Minimax AI, and input/output handling
import Game
import Minimax
    ( playAI
    )

import System.IO
    ( hFlush
    , stdout
    )
import Data.List
    ( intercalate
    )
import Text.Printf
    ( printf
    )
import Text.Read
    ( readMaybe
    )

-- Function to display the current state of the game board
displayBoard :: BigBoard -> Int -> IO ()
displayBoard bb hl = putStrLn $ concat (replicate 32 "\n") ++ intercalate "\n" [line n | n <- [0..14]]
    where
        line :: Int -> String
        line n
            | n > 13            = ""  -- Ends board display
            | n == 4 || n == 9  = "----------|----------|----------"  -- Divider lines between boards
            | l == 0            = formatLine " %s        " [if hl `elem` [-1, i] then show (i + 1) else " " | i <- indices]  -- Numbering for boards
            | l >= 1            = formatLine "   %s " [squareLine (l - 1, i) (bb !! i) | i <- indices]  -- Display squares
            | otherwise         = "          |          |          "  -- Padding lines
                where
                    l = mod n 5
                    formatLine f lst = intercalate "|" $ printf f <$> lst
                    indices = [div n 5 * 3 + i | i <- [0..2]]  -- Board indices
        squareLine :: (Int, Int) -> Square -> String
        squareLine (n, m) sq = case sq of
            (Win X) | n == 0    -> "  \\ / "
                    | n == 1    ->  "   X  "
                    | n == 2    -> "  / \\ "
            (Win O) | n == 0    -> "  / \\ "
                    | n == 1    ->  "  | | "
                    | n == 2    -> "  \\ / "
            (Board b)           -> concat [" " ++ maybe (plc i) show c | o <- [0..2], let i = 3 * n + o, let c = b !! i]
                where plc i
                        | m == hl   = show (i + 1)
                        | otherwise = " "
            _                   -> "      "

-- Main game loop that handles game state and user interaction
gameLoop :: String -> State -> IO ()
gameLoop msg state@(State b p r) = do
    displayBoard b r  -- Display the current board

    -- Prompt player for move and handle input
    putStr msg
    putStrLn $ "You are currently playing as " ++ show p ++ ".\n"

    choice <- chooseBoard b r  -- Let player choose which board to play on

    putStrLn $ "You are playing on board " ++ show choice ++ "."
    putStr "Enter your move (1-9): "
    hFlush stdout  -- Ensure prompt is displayed immediately

    move <- getLine  -- Read player's move

    -- Process the move and update the game state accordingly
    case readMaybe move of
        Just a
            | isSquarePlayable (b !! (choice - 1)) (a - 1) -> do
                let act = Action (choice - 1) (a - 1)
                case play state act of
                    Continue (State nb _ _) -> displayBoard nb (-1)
                    _ -> return ()
                putStrLn "Please hold on..."
                (m, ns) <- case playAI state act of
                    Continue s
                        -> return ("", s)
                    End (Just winner) s
                        -> return (show winner ++ " wins! Starting a new game...\n", s)
                    End Nothing s
                        -> return ("It's a draw! Starting a new game...\n", s)
                gameLoop m ns
            | a == -1 -> return ()
            | otherwise -> gameLoop ("You cannot move in square " ++ show a ++ "! Please try again.\n") state
        Nothing -> gameLoop "Your move is invalid! Please try again.\n" state

-- Function to allow the player to choose a board to play on
chooseBoard :: BigBoard -> BoardChoice -> IO Int
chooseBoard b (-1) = do
    putStr "You may choose a board to play on (1-9): "
    hFlush stdout
    choice <- getLine
    case readMaybe choice of
        Just c  | c > 0 && c <= 9 && isSquarePlayable (b !! (c - 1)) (-1)
            -> do
                displayBoard b (c - 1)
                return c
        _   -> do
                displayBoard b (-1)
                putStrLn "Entered value is invalid! Please try again.\n"
                chooseBoard b (-1)
chooseBoard _ r = return (r + 1)

-- Entry point of the program; initializes the game
main :: IO ()
main = do
  gameLoop "Welcome to Ultimate Tic-Tac-Toe!\n" initialState  -- Start the game loop with the initial state
