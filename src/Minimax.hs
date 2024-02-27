module Minimax
    ( find
    , findExtreme
    , findFast
    , findNoob
    , findc
    , playAI
    , playBest
    ) where

import Game

import Data.List
    ( sortBy
    )

type Val = (Double, [(Player, BoardAction)])

findExtreme :: State -> (Double, Action)
findExtreme = findc (9, 9)

find :: State -> (Double, Action)
find = findc (6, 9)

findFast :: State -> (Double, Action)
findFast = findc (5, 7)

findNoob :: State -> (Double, Action)
findNoob = findc (3, 5)

findc :: (Int, Int) -> State -> (Double, Action)
findc (dmin, dmax) (State sq p o)
    | o == -1 = head (sortBy f [extm n (boardVal p (sq!!n, p, dmin, nonfv, [])) | n <- validsq])
    | otherwise = extm o (boardVal p (sq!!o, p, dmax, nonfv, []))
        where
            validsq = [n | n <- [0..8], isSquarePlayable (sq!!n) (-1)]
            nonfv   = (-1):[n | n <- [0..8], let s = sq!!n, not (isSquarePlayable s (-1)) || abs (advantage p s) > 1]
            extm bc (v, pth)
                    = (v, Action bc (snd (head pth)))
            f (a, _) (b, _)
                    = compare b a

playBest :: Result -> Result
playBest (Continue s) = play s (snd (find s))
playBest r = r

playAI :: State -> Action -> Result
playAI s a = playBest (play s a)

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
