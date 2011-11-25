module Reversi.Strategies where

import Reversi.Game

type Strategy = Board -> Player -> Position

greedy :: Strategy
greedy board@(Board grid) player = best
    where moves = possibleMoves board player
          best = foldr (\x y -> if (better x y) then x else y) (head moves) (tail moves)
          better x y = (score (play board player x) player) > (score (play board player y) player)


--
-- evaporation
--
--   Arguments:
--     Strategy
--
--   Comments:
--     To be used with mobility strategy. This strategy tries to keep the
--     number of pieces down during the first stage of the game, so as not to
--     lose a massive block of pieces later in the game.
--
--     This function simply finds the minimum path until move 17, when it will
--     switch to the maximum value path. In essence, this is greedy.
--
--   TODO:
--     Make the look-ahead go deeper.
--     Put in a guard to avoid giving a complete win to the opponent.
--

evaporation :: Strategy
evaporation board@(Board grid) player
    | opening   = worst
    | otherwise = best
    where best = foldr (\x y -> if (better x y) then x else y)
                       (head moves) (tail moves)
          better x y = (score (play board player x) player) 
                     > (score (play board player y) player)
          moves = possibleMoves board player
          worst = foldr (\x y -> if (better x y) then y else x)
                        (head moves) (tail moves)
          opening = if (move board < 19) then True else False
