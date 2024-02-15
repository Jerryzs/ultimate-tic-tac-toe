module Game
    ( Action (..)
    , start
    ,
    ) where

import Data.Maybe

data Player = X
            | O
        deriving (Eq, Show)

data Square = Win Player
            | Board [Maybe Player]
        deriving (Eq, Show)

data Action = Action Int Int
        deriving (Eq, Show)

{-| Game state which consists of a list of nine squares, the player to play, and
    the location of the board that the player is permitted to play.
-}
data State  = State [Square] Player Int
        deriving (Eq, Show)

initialState = State (replicate 9 (Board (replicate 9 Nothing))) X (-1)

start :: IO ()
start = putStrLn "hello world" -- TODO
