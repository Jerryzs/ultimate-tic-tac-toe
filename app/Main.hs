module Main (main) where

import Game
import Minimax
    ( playAI
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
            | n > 13            = ""
            | n == 4 || n == 9  = "----------|----------|----------"
            | l == 0            = formatLine " %d        " (offsetIndices 1)
            | l >= 1            = formatLine "   %s " [squareLine (l - 1, i) (bb !! i) | i <- offsetIndices 0]
            | otherwise         = "          |          |          "
                where
                    l = mod n 5
                    formatLine f lst = intercalate "|" $ printf f <$> lst
                    offsetIndices o = [div n 5 * 3 + i | i <- [(0 + o)..(2 + o)]]
        squareLine :: (Int, Int) -> Square -> String
        squareLine (n, m) sq = case sq of
            (Win X) | n == 0    -> "  \\ / "
                    | n == 1    ->  "   X  "
                    | n == 2    -> "  / \\ "
            (Win O) | n == 0    -> "  / \\ "
                    | n == 1    ->  "  | | "
                    | n == 2    -> "  \\ / "
            (Board b)           -> concat [" " ++ maybe (plc i) show c | o <- [0..2], let i = 3 * n + o, let c = b !! i]
                where   plc i   | m == hl = show (i + 1)
                                | otherwise = " "
            _                   -> "      "

-- Main game loop
gameLoop :: String -> State -> IO ()
gameLoop msg state@(State b p r) = do
    displayBoard b r

    putStr msg
    putStrLn $ "You are currently playing as " ++ show p ++ ".\n"

    choice <- chooseBoard b r

    putStrLn $ "You are playing on board " ++ show choice ++ "."
    putStr "Enter your move (1-9): "

    move <- getLine

    putStrLn ""
    putStrLn "Please hold on..."

    case readMaybe move of
        Just a
            | isSquarePlayable (b !! (choice - 1)) (a - 1) -> do
                (m, ns) <- case playAI state (Action (choice - 1) (a - 1)) of
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

chooseBoard :: BigBoard -> BoardChoice -> IO Int
chooseBoard b (-1) = do
    putStr "You may choose a board to play on (1-9): "
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

main :: IO ()
main = do
  gameLoop "Welcome to Ultimate Tic-Tac-Toe!\n" initialState
