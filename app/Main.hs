module Main (main) where

import Game

main :: IO ()
main = do
    let _ = start (Action 0 0)
    return ()
