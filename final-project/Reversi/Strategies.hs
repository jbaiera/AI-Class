module Reversi.Strategies where

import Reversi.Game

type Strategy = Board -> Player -> Position

-- Confidence should be in the range of 0.0 to 100.0.
type Confidence = Float
type Depth = Int

type Weighting = Board -> Player -> [(Position, Confidence)]

type Heuristic = Board -> Player -> Position -> Int

greedy :: Strategy
greedy board@(Board grid) player = best
    where moves = possibleMoves board player
          best = foldr (\x y -> if (better x y) then x else y) (head moves) (tail moves)
          better x y = (score (play board player x) player) > (score (play board player y) player)

minimax2 :: Strategy
minimax2 = minimax 2 greedyEval

minimax3 :: Strategy
minimax3 = minimax 3 greedyEval

minimax4 :: Strategy
minimax4 = minimax 4 greedyEval

minimax5 :: Strategy
minimax5 = minimax 5 greedyEval

minimax8 :: Strategy
minimax8 = minimax 8 greedyEval

greedyEval :: Heuristic
greedyEval board player position = score (play board player position) player

minimax :: Depth -> Heuristic -> Strategy
minimax depth heuristic board player = bestMove
    where moves = possibleMoves board player
          scores = [ (score, position) | position <- moves, let score = minimax' depth heuristic player (play board player position) player ]
          bestMove = snd $ maximum scores

minimax' :: Depth -> Heuristic -> Player -> Board -> Player -> Int
minimax' depth heuristicFunction maxPlayer board currPlayer
    | depth == 0    = heuristicFunction board maxPlayer pass
    | otherwise     = best
    where moves = possibleMoves board currPlayer
          nextPlayer = getOpponent currPlayer
          childWeights = map (\m -> minimax' (depth-1) heuristicFunction maxPlayer (play board currPlayer m) nextPlayer) moves
          best = choose childWeights
          choose | currPlayer == maxPlayer  = maximum
                 | otherwise                = minimum

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
