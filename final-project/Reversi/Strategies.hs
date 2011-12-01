module Reversi.Strategies where

import Reversi.Game

type Strategy = Board -> Player -> Position

-- Confidence should be in the range of 0.0 to 100.0.
type Confidence = Float

type Weighting = Board -> Player -> [(Position, Confidence)]

--
-- testWeighting
--
-- This function is being used to test the new data type Weighting.
--

testWeighting :: Weighting
testWeighting grid player = [((1,2), 22.4)]

greedyWeighting :: Weighting
greedyWeighting board player = weights
    where moves = possibleMoves board player
          weights = [ (pos, weight) | pos <- moves, let weight = fromIntegral (score (play board player pos) player) ]


--
greedy :: Strategy
greedy board@(Board grid) player = best
    where moves = possibleMoves board player
          best = foldr (\x y -> if (better x y) then x else y) (head moves) (tail moves)
          better x y = (score (play board player x) player) > (score (play board player y) player)


--
-- evaporation
--
--   To be used with mobility strategy. This strategy tries to keep the
--   number of pieces down during the first stage of the game, so as not to
--   lose a massive block of pieces later in the game.
--
--   This function simply finds the minimum path until move 17, when it will
--   switch to the maximum value path. In essence, this is greedy.
--
--   Arguments
--     (Strategy)
--
--   TODO
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

--
-- mobility
--
--   This function tries to increase the number of playable moves while
--   decreasing them for the opponent.
--
--   Arguments
--     (Strategy)
--       Board:
--       Player:
--     
--   Return
--     (Strategy)
--       Position: 
--
--   TODO
--     Trace through to see if a player can guess the move of the opponent.  
--

mobility :: Strategy
mobility board@(Board grid) player = best
    where moves = possibleMoves board player
          best = foldr (\x y -> if (better x y) then x else y)
                       (head moves) (tail moves)
          better x y = (length $ possibleMoves (play board player x) player)
                     > (length $ possibleMoves (play board player y) player)
