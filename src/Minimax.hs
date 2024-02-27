module Minimax
    ( find
    , playAI
    , playBest
    ) where

import Game

import Data.List
    ( sortBy
    )

type Val = (Double, [(Player, BoardAction)])

find :: State -> (Double, Action)
find (State sq p o)
    | o == -1 = head (sortBy f [extm n (boardVal p (sq!!n, p, 5, nfv, [])) | n <- ps])
    | otherwise = extm o val
        where
            ps      = [n | n <- [0..8], isSquarePlayable (sq!!n) (-1)]
            nfv     = [n | n <- [0..8], let s = sq!!n, not (isSquarePlayable s (-1)) || abs (advantage p s) > 1]
            val     = boardVal p (sq!!o, p, 9, nfv, [])
            extm bc (v, pth)
                    = (v, Action bc (snd (head pth)))
            f (a, _) (b, _)
                    = compare b a

playBest :: State -> State
playBest s = play (s, snd (find s))

playAI :: (State, Action) -> State
playAI (s, a) = playBest (play (s, a))

advantage :: Player -> Square -> Int
advantage _ (Win _) = 0
advantage p (Board b) = uncurry (-) c
    where   f x y   = case x of
                Just a | a == p -> (fst y + 1, snd y)
                Just _          -> (fst y, snd y + 1)
                Nothing         -> y
            c       = foldr f (0, 0) b

boardVal :: Player -> (Square, Player, Int, [Int], [(Player, BoardAction)]) -> Val
boardVal wp (Win p, _, _, _, _)
    | wp == p   = ( 100, [])
    | otherwise = (-100, [])
boardVal wp (Board b, p, n, unfv, path)
    | n < 1             = (  0, path)
    | bw == Just wp     = (  1 * (fromIntegral n), path)
    | bw == Just np     = ( -1 * (fromIntegral n), path)
    | null acts         = (0.5, path)
    | length acts == 1  = (0.5, path ++ [(p, head acts)]) -- maybe redundant
    | otherwise         = foldr minimax (head valpths) valpths
        where   bw      = getBoardWinner b
                np      = nextp wp
                acts    = getBoardActions b
                nb i    = case splitAt i b of
                    (bf, _:af)  -> Board (bf ++ Just p: af)
                    _           -> Board (take 8 b ++ [Just p]) -- theoretically unreachable
                valpths = [boardVal wp (nb i, nextp p, n - 1, [], path ++ [(p, i)]) | i <- acts]
                bias (v, pth)
                    | v == 0 || null pth            = (      v, pth)
                    | a `elem` unfv                 = (  m * v, pth)
                    | (-1) `elem` unfv && a == 4    = (0.1 + v, pth)
                    | otherwise                     = (      v, pth)
                        where   a   = snd (head pth)
                                m   = if v < 0 then 1.2 else 0.8
                op
                    | wp == p   = (>)
                    | otherwise = (<)
                minimax x y
                    | fst x /= 0 && fst bx `op` fst y   = bx
                    | otherwise                         = y
                        where   bx = bias x
