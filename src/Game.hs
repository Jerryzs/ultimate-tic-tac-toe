module Game (module Game) where

import Data.Maybe
    ( fromJust
    , isJust
    , isNothing
    )

data Player = X
            | O
        deriving (Eq, Show)

type Board = [Maybe Player]

type BoardChoice = Int

type BoardAction = Int

data Square = Win Player
            | Board Board
        deriving (Eq, Show)

type BigBoard = [Square]

data Action = Action BoardChoice BoardAction
        deriving (Eq, Show)

{-| Game state which consists of a list of nine squares, the player to play, and
    the location of the board that the player is permitted to play.
-}
data State  = State BigBoard Player BoardChoice
        deriving (Eq, Show)

initialState :: State
initialState = State (replicate 9 (Board (replicate 9 Nothing))) X (-1)

checkAction :: (State, Action) -> Bool
checkAction (State s _ r, Action o i) =
    (r == -1 || o == r)
    && isSquarePlayable (s!!o) i

getActions :: State -> [Action]
getActions s = [a | o <- [0..8], i <- [0..8], let a = Action o i, checkAction (s, a)]

getBoardActions :: Board -> [BoardAction]
getBoardActions b = [i | i <- [0..8], isNothing (b!!i)]

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

draw :: State -> Bool
draw s = fst r && isNothing (snd r)
    where r = result s

win :: State -> Bool
win s = fst r && isJust (snd r)
    where r = result s

winner :: State -> Maybe Player
winner s = snd (result s)

result :: State -> (Bool, Maybe Player)
result (State s _ r)
    | r /= (-1)                                         = (False, Nothing)
    | isJust wp                                         = (True, wp)
    | and [not (isSquarePlayable sq (-1)) | sq <- s]    = (True, Nothing)
    | otherwise                                         = (False, Nothing)
        where wp = getWinner s

nextp :: Player -> Player
nextp X = O
nextp O = X

execute :: (BigBoard, Player, Action) -> BigBoard
execute (s, p, Action o i) = case splitAt o s of
    (bf, _:af) -> bf ++ updateSquare (executeSquare (s!!o, p, i)):af
    _ -> s -- theoretically unreachable

executeSquare :: (Square, Player, Int) -> Square
executeSquare (Board b, p, i) = case splitAt i b of
    (bf, _:af) -> Board (bf ++ Just p: af)
    _ -> Board (take 8 b ++ [Just p]) -- theoretically unreachable
executeSquare (Win p, _, _) = Win p

updateSquare :: Square -> Square
updateSquare (Win p) = Win p
updateSquare (Board b)
    | isNothing wp  = Board b
    | otherwise     = Win (fromJust wp)
        where wp = getBoardWinner b

getWinner :: BigBoard -> Maybe Player
getWinner s = getBoardWinner (map f s)
    where f sq = case sq of
                    Win p -> Just p
                    Board _ -> Nothing

getBoardWinner :: Board -> Maybe Player
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
                np = nextp p
                nr
                    | isSquarePlayable (ns!!i) (-1) = i
                    | otherwise = -1

start :: Action -> State
start (Action a b) = play (initialState, Action a b)
