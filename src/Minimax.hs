module Minimax
    ( find
    , findExtreme
    , findFast
    , findNoob
    , findc
    , playAI
    , playBest
    ) where

-- Importing necessary modules for game logic and randomness
import Game

import System.Random
    ( mkStdGen
    , uniformR
    )
import Data.List
    ( sortBy
    , unfoldr
    )

type Val = (Double, [(Player, BoardAction)])

-- Different find functions provide varying levels of depth for the Minimax algorithm.
findExtreme :: State -> (Double, Action)
findExtreme = findc (9, 9)  -- Uses a high depth for thorough search.

find :: State -> (Double, Action)
find = findc (4, 9)         -- Standard search depth.

findFast :: State -> (Double, Action)
findFast = findc (4, 6)     -- Reduced depth for faster computation.

findNoob :: State -> (Double, Action)
findNoob = findc (2, 4)     -- Low depth for a less challenging AI.

{-|
    Finds the best action to play, given the current state. If the state imposes
    a small board, the function returns the value computed from the only legal
    small board. If the state allows free choice of small board, values are
    computed for all legal small boards, then a random move is chosen from a
    list of best moves, which must have equal values.
-}
findc :: (Int, Int) -> State -> (Double, Action)
findc (dmin, dmax) (State sq p o)
    | o == -1   = rand $ sortBy f ([extm n (boardVal p dmin (sq, n)) | n <- validsq])
    | otherwise = extm o (boardVal p dmax (sq, o))
        where
            validsq     = [n | n <- [0..8], isSquarePlayable (sq!!n) (-1)]
            extm bc (v, pth)
                        = (v, Action bc (snd (head pth)))
            f (a, _) (b, _)
                        = compare b a
            rand l
                | length best == 1  = head best
                | otherwise         = best !! (rng !! (length l - length best))
                    where
                        best = foldl g [] l
                        g (h@(bv, _):t) m@(v, _)
                            | bv == v = m:h:t
                            | otherwise = h:t
                        g [] a = [a]
                        rng = unfoldr (Just . uniformR (0, length best - 1)) $ mkStdGen $ length l

-- Selects the best move based on the current game state.
playBest :: Result -> Result
playBest (Continue s) = play s (snd (find s))  -- Continues playing if the game isn't over.
playBest r = r                                 -- Returns the result if the game has ended.

{-|
    Plays the given action on the given state, then plays the computed best move
    from @playBest@. Returns the result of the game after the computed move is
    played, or, if the game concludes before a move can be computed, the final
    result of the game with no move made by the computer.
-}
playAI :: State -> Action -> Result
playAI s a = playBest (play s a)  -- Plays the best move after the given action.

{-|
    Returns an integer specifying the number of moves the given player has made
    on a square subtracted by the number of moves made by the opponent.
-}
advantage :: Player -> Square -> Int
advantage _ (Win _) = 0
advantage p (Board b) = uncurry (-) c
    where   f x y   = case x of
                Just a | a == p -> (fst y + 1, snd y)
                Just _          -> (fst y, snd y + 1)
                Nothing         -> y
            c       = foldr f (0, 0) b

{-|
    Computes the value for a square on the main board, given the player whose
    turn it is to play and the maximum number of total turns to be played on
    this board. For a square that is a small board, assuming the opponent always
    plays the best move: a positive value indicates the possibility for the
    player to win the small board, a negative value eliminates such possibility;
    the closer the value is to the given depth, the closer the player is to
    winning. A list of tuples, specifying the player and the move to play, is
    returned with the computed value to represent the  sequence of turns that
    needs to be played to reach the evaluated outcome.
-}
boardVal :: Player -> Int -> (BigBoard, BoardChoice) -> Val
boardVal player depth (board, choice) = f True player (board !! choice, board, choice, player, depth, []) -- Recursive function to evaluate board value.
    where
        f _ wp (Win p, _, _, _, _, _)
            | wp == p   = ( 4, [])
            | otherwise = (-4, [])
        f entry wp (Board b, cxt, c, p, n, path)
            | b == emptyb       = foldr minimax (bias $ head blnkvps) blnkvps
            | bw == Just wp     = (  1 * fromIntegral n, path)
            | bw == Just np     = ( -1 * fromIntegral n, path)
            | null acts         = (0.5, path)
            | n < 1             = (  0, path)
            | otherwise         = foldr minimax (bias $ head valpths) valpths
                where
                    emptyb  = replicate 9 Nothing
                    bw      = getBoardWinner b
                    np      = nextp wp
                    acts    = getBoardActions b
                    nb i    = case splitAt i b of
                        (bf, _:af)  -> Board (bf ++ Just p: af)
                        _           -> Board (take 8 b ++ [Just p]) -- theoretically unreachable
                    blnkvps = [(0.5, [(p, i)]) | i <- [0..3] ++ [5..8]] ++ [(0.6, [(p, 4)])]
                    valpths = [f False wp (nb i, cxt, c, nextp p, n - 1, path ++ [(p, i)]) | i <- acts]
                    bias (v, pth)
                        | null pth                          = (    v, pth)
                        | not $ isSquarePlayable nxtsq (-1) = (m 2 v, pth)
                        | entry && v2 /= 0                  = (deepe, pth)
                        | otherwise                         = (mad v, pth)
                            where
                                nxt     = snd (head pth)
                                nxtsq   = execute (cxt, p, Action c nxt) !! nxt
                                v2      = fst $ f False p (nxtsq, cxt, c, nextp p, n, [])
                                -- v3      = fst $ boardVal False p (nxtsq, cxt, c, p, 4, [])
                                deepe   = (v + v2) * 2
                                m :: Int -> Double -> Double
                                m l a
                                    | a < 0     = a * (1.2 ** (- fromIntegral l))
                                    | otherwise = a * (0.83 ** fromIntegral l)
                                mad     = m (advantage p nxtsq)
                    op
                        | wp == p   = (>)
                        | otherwise = (<)
                    minimax x y
                        | fst x /= 0 && fst bx `op` fst y   = bx
                        | otherwise                         = y
                            where   bx = bias x
