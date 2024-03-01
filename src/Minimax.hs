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

import Data.Foldable
    ( maximumBy
    )

type Val = (Double, [(Player, BoardAction)])

findExtreme :: State -> (Double, Action)
findExtreme = findc (9, 9)

find :: State -> (Double, Action)
find = findc (6, 9)

findFast :: State -> (Double, Action)
findFast = findc (4, 6)

findNoob :: State -> (Double, Action)
findNoob = findc (2, 4)

findc :: (Int, Int) -> State -> (Double, Action)
findc (dmin, dmax) (State sq p o)
    | o == -1   = maximumBy f ([extm n (boardVal p dmin (sq, n)) | n <- validsq])
    | otherwise = extm o (boardVal p dmax (sq, o))
        where
            validsq = [n | n <- [0..8], isSquarePlayable (sq!!n) (-1)]
            extm bc (v, pth)
                    = (v, Action bc (snd (head pth)))
            f (a, _) (b, _)
                    = compare a b

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

boardVal :: Player -> Int -> (BigBoard, BoardChoice) -> Val
boardVal player depth (board, choice) = f True player (board !! choice, board, choice, player, depth, [])
    where
        f _ wp (Win p, _, _, _, _, _)
            | wp == p   = ( 4, [])
            | otherwise = (-4, [])
        f entry wp (Board b, cxt, c, p, n, path)
            | null acts         = (0.5, path)
            | b == emptyb       = foldr minimax (bias $ head blnkvps) blnkvps
            | n < 1             = (  0, path)
            | bw == Just wp     = (  1 * fromIntegral n, path)
            | bw == Just np     = ( -1 * fromIntegral n, path)
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
