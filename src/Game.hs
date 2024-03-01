module Game (module Game) where


import Data.Maybe
    ( fromJust
    , isJust
    , isNothing
    )

-- Definition of the Player data type with two possible values: X and O
data Player = X
            | O
        deriving (Eq, Show)

-- Type aliases for clearer code. Board is a list of Maybe Player, representing the state of a tic-tac-toe board.
type Board = [Maybe Player]

-- Type alias for choices on the board, essentially an index.
type BoardChoice = Int

-- Same as BoardChoice, used for actions on the board.
type BoardAction = Int

-- Square can either be a Win by a player or a Board with its current state.
data Square = Win Player
            | Board Board
        deriving (Eq, Show)

-- BigBoard is a list of Squares, representing the larger tic-tac-toe board where each square can be a mini tic-tac-toe game.
type BigBoard = [Square]

-- Action data type represents a move with a choice of which mini-board and where on that board.
data Action = Action BoardChoice BoardAction
        deriving (Eq, Show)

{-| Game state which consists of a list of nine squares, the player to play, and
    the location of the board that the player is permitted to play.
-}
data State  = State BigBoard Player BoardChoice
        deriving (Eq, Show)

-- Result of an action, could be continuation of the game or an end with a possible winner.
data Result = Continue State
            | End (Maybe Player) State
        deriving (Eq, Show)

-- Initial state of the game with an empty board and X starting.
initialState :: State
initialState = State (replicate 9 (Board (replicate 9 Nothing))) X (-1)

-- Check if an action is valid given the current state.
checkAction :: (State, Action) -> Bool
checkAction (State s _ r, Action o i) =
    (r == -1 || o == r)
    && isSquarePlayable (s!!o) i

-- Get all valid actions given the current state.
getActions :: State -> [Action]
getActions s = [a | o <- [0..8], i <- [0..8], let a = Action o i, checkAction (s, a)]

-- Get valid actions for a single board.
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

-- Determine the result of the game: continue, win, or draw.
result :: State -> Result
result (State s p r)
    | isJust wp                                         = End wp initialState
    | and [not (isSquarePlayable sq (-1)) | sq <- s]    = End Nothing initialState
    | otherwise                                         = Continue (State s p r)
        where wp = getWinner s

-- Determine the next player.
nextp :: Player -> Player
nextp X = O
nextp O = X

-- Execute an action on the big board.
execute :: (BigBoard, Player, Action) -> BigBoard
execute (s, p, Action o i) = case splitAt o s of
    (bf, _:af) -> bf ++ updateSquare (executeSquare (s!!o, p, i)):af
    _ -> s -- theoretically unreachable

-- Execute an action on a single square.
executeSquare :: (Square, Player, Int) -> Square
executeSquare (Board b, p, i) = case splitAt i b of
    (bf, _:af) -> Board (bf ++ Just p: af)
    _ -> Board (take 8 b ++ [Just p]) -- theoretically unreachable
executeSquare (Win p, _, _) = Win p

-- Update the state of a square after an action.
updateSquare :: Square -> Square
updateSquare (Win p) = Win p
updateSquare (Board b)
    | isNothing wp  = Board b
    | otherwise     = Win (fromJust wp)
        where wp = getBoardWinner b

-- Get the winner of the big board.
getWinner :: BigBoard -> Maybe Player
getWinner s = getBoardWinner (map f s)
    where f sq = case sq of
                    Win p -> Just p
                    Board _ -> Nothing

-- Get the winner of a single board.
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

-- Play a game starting with an initial action.
play :: State -> Action -> Result
play (State s p r) (Action o i)
    | checkAction (State s p r, Action o i) = result (State ns np nr)
    | otherwise                             = Continue (State s p r)
        where   ns = execute (s, p, Action o i)
                np = nextp p
                nr
                    | isSquarePlayable (ns!!i) (-1) = i
                    | otherwise = -1

-- Start a game with an initial action.
start :: Action -> Result
start (Action a b) = play initialState (Action a b)
