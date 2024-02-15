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

checkAction :: (State, Action) -> Bool
checkAction (State s _ r, Action o i) =
    (r == -1 || o == r)
    && isSquarePlayable (s!!o) i

{-| Checks if a player can play in a given square and, if desired, at a given
    location. The location should be an integer from 0 to 8 (incl.), or -1 to
    indicate that only the playability of the overall square is to be checked.

    Returns False if the square is already won (claimed), if the location is not
        in [0, 8], or if the location is not -1 and is occupied (has been played
        in); otherwise, returns True.
-}
isSquarePlayable :: Square -> Int -> Bool
isSquarePlayable (Win _) _ = False
isSquarePlayable (Board l) i
        | i >= 0 && i < 9   = isNothing (l!!i)
        | i == -1           = True
        | otherwise         = False

play :: (State, Action) -> State
play (State s p r, Action o i) = initialState

start :: Action -> State
start (Action a b) = play (initialState, Action a b)
