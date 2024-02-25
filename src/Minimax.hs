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
        where   v   = boardVal p (sq!!o, p, 9, [])

type Val = (Double, [(Player, BoardAction)])

boardVal :: Player -> (Square, Player, Int, [(Player, BoardAction)]) -> Val
boardVal wp (Win p, _, _, _)
    | wp == p   = ( 100, [])
    | otherwise = (-100, [])
boardVal wp (Board b, p, n, path)
    | n < 1             = (  0, path)
    | bw == Just wp     = (  1 * (fromIntegral n), path)
    | bw == Just np     = ( -1 * (fromIntegral n), path)
    | length acts <= 1  = (0.5, path)
    | otherwise         = foldr minimax (valpths!!0) valpths
        where   bw      = getBoardWinner b
                np      = nextp wp
                acts    = getBoardActions b
                nb i    = case splitAt i b of
                    (bf, _:af)  -> Board (bf ++ Just p: af)
                    _           -> Board (take 8 b ++ [Just p]) -- theoretically unreachable
                valpths = [boardVal wp (nb i, nextp p, n - 1, path ++ [(p, i)]) | i <- acts]
                op
                    | wp == p   = (>)
                    | otherwise = (<)
                minimax x y
                    | fst x /= 0 && fst x `op` fst y    = x
                    | otherwise                         = y
