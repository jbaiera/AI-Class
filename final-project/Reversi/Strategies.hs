module Reversi.Strategies where

import Reversi.Game

type Strategy = Board -> Player -> Position

greedy :: Strategy
greedy board@(Board grid) player = best
    where moves = possibleMoves board player
          best = foldr (\x y -> if (better x y) then x else y) (head moves) (tail moves)
          better x y = (score (play board player x) player) > (score (play board player y) player)

