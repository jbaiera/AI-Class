module Reversi.Interface where

import Reversi.Game
import Reversi.Strategies

-- plays a game between two functions, printing the state of the board each time
simulate :: Board -> Player -> Strategy -> Strategy -> IO ()
simulate board player strategy1 strategy2
    | possibleMoves board player == []  = prettyPrint board
    | player == 1  = do prettyPrint board
                        simulate (play board 1 $ strategy1 board player) 2 strategy1 strategy2
    | otherwise    = do prettyPrint board
                        simulate (play board 2 $ strategy2 board player) 1 strategy1 strategy2


