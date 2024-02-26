module Minimax
    ( find
    ) where

import Game
    ( Action (..)
    , BigBoard
    , Board
    , BoardAction
    , BoardChoice
    , Player
    , Square (..)
    , State (..)
    , checkAction
    , draw
    , getActions
    , getBoardActions
    , getBoardWinner
    , initialState
    , nextp
    , play
    , result
    , start
    , win
    , winner
    )

find :: State -> (Double, BoardAction)
find (State sq p o)
    | o == -1 = (0.0, 0)
    | otherwise = (fst v, snd (head (snd v)))
        where   v   = boardVal p (sq!!o, p, 9, [], [])

type Val = (Double, [(Player, BoardAction)])

boardVal :: Player -> (Square, Player, Int, [Int], [(Player, BoardAction)]) -> Val
boardVal wp (Win p, _, _, _, _)
    | wp == p   = ( 100, [])
    | otherwise = (-100, [])
boardVal wp (Board b, p, n, unfv, path)
    | n < 1             = (  0, path)
    | bw == Just wp     = (  1 * (fromIntegral n), path)
    | bw == Just np     = ( -1 * (fromIntegral n), path)
    | length acts <= 1  = (0.5, path)
    | otherwise         = foldr minimax (head valpths) valpths
        where   bw      = getBoardWinner b
                np      = nextp wp
                acts    = getBoardActions b
                nb i    = case splitAt i b of
                    (bf, _:af)  -> Board (bf ++ Just p: af)
                    _           -> Board (take 8 b ++ [Just p]) -- theoretically unreachable
                valpths = [boardVal wp (nb i, nextp p, n - 1, [], path ++ [(p, i)]) | i <- acts]
                op
                    | wp == p   = (>)
                    | otherwise = (<)
                {-| Extracts value from type Val and applies bias, where moves
                    that give the opponent favourable positions are considered
                    less valueable to the current player and more valueable to
                    the opponent. The favourable positions (board choices) for
                    the opponent are supplied in @unfv@.
                -}
                extv (v, pth)
                    | v == 0 || null pth || not (a `elem` unfv) = v
                    | otherwise                                 = m * v
                        where   a   = snd (head pth)
                                m   = if v < 0 then 1.2 else 0.8
                    -- case pth of
                    --         (_:(_, a):_) | a `elem` pref    -> fst m * v
                    --         ((_, a):_:_) | a `elem` pref    -> snd m * v
                    --         _                               -> v
                    --     where   m   = if v > 0 then (1.2, 0.8) else (0.8, 1.2)
                minimax x y
                    | fst x /= 0 && extv x `op` extv y  = x
                    | otherwise                         = y
