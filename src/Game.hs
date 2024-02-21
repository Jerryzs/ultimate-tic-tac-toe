module Game
    ( Action (..)
    , State
    , checkAction
    , initialState
    , start
    , winner
    ) where

import Data.Maybe ( fromJust, isJust, isNothing )

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

initialState :: State
initialState = State (replicate 9 (Board (replicate 9 Nothing))) X (-1)

checkAction :: (State, Action) -> Bool
checkAction (State s _ r, Action o i) =
    (r == -1 || o == r)
    && isSquarePlayable (s!!o) i

{-| Checks if a player can play in a given square and, if desired, at a given
    location. The location should be an integer from 0 to 8 (incl.), or -1 to
    indicate that only the playability of the overall square is to be checked.

    Returns @False@ if the square is already won (claimed), if the location is
    not in @[0, 8]@, or if the location is not -1 and is occupied (has been
    played in); otherwise, returns @True@.
-}
isSquarePlayable :: Square -> Int -> Bool
isSquarePlayable (Win _) _ = False
isSquarePlayable (Board l) i
        | i >= 0 && i < 9   = isNothing (l!!i)
        | i == -1           = Nothing `elem` l
        | otherwise         = False

getNextPlayer :: Player -> Player
getNextPlayer X = O
getNextPlayer O = X

execute :: ([Square], Player, Action) -> [Square]
execute (s, p, Action o i) = case splitAt o s of
    (bf, _:af) -> bf ++ updateSquare (executeSquare (s!!o, p, i)):af
    _ -> s

executeSquare :: (Square, Player, Int) -> Square
executeSquare (Board b, p, i) = case splitAt i b of
    (bf, _:af) -> Board (bf ++ Just p: af)
    _ -> Board (replicate 9 Nothing)
executeSquare (Win p, _, _) = Win p

updateSquare :: Square -> Square
updateSquare (Win p) = Win p
updateSquare (Board b)
    | isNothing win = Board b
    | otherwise     = Win (fromJust win)
        where win = getBoardWinner b

winner :: State -> Maybe Player
winner (State s _ _) = getWinner s

getWinner :: [Square] -> Maybe Player
getWinner s = getBoardWinner (map f s)
    where f sq = case sq of
                    Win p -> Just p
                    Board _ -> Nothing

getBoardWinner :: [Maybe Player] -> Maybe Player
getBoardWinner [tl, tc, tr, cl, cc, cr, bl, bc, br]
    | isJust tc && tc == tl && tc == tr = tc
    | isJust cr && cr == tr && cr == br = cr
    | isJust bc && bc == bl && bc == br = bc
    | isJust cl && cl == tl && cl == bl = cl
    | isJust cc && cc == tc && cc == bc = cc
    | isJust cc && cc == cl && cc == cr = cc
    | isJust cc && cc == tl && cc == br = cc
    | isJust cc && cc == tr && cc == bl = cc
    | otherwise                         = Nothing
getBoardWinner lst
    | length lst > 9    = getBoardWinner (take 9 lst)
    | otherwise         = Nothing

play :: (State, Action) -> State
play (State s p r, Action o i)
    | checkAction (State s p r, Action o i) = State ns np nr
    | otherwise                             = State s p r
        where   ns = execute (s, p, Action o i)
                np = getNextPlayer p
                nr
                    | isSquarePlayable (ns!!i) (-1) = i
                    | otherwise = -1

start :: Action -> State
start (Action a b) = play (initialState, Action a b)
