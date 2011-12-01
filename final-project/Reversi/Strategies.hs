module Reversi.Strategies where

import Reversi.Game

type Strategy = Board -> Player -> Position

-- Confidence should be in the range of 0.0 to 100.0.
type Confidence = Float

type Weighting = Board -> Player -> [(Position, Confidence)]

greedyWeighting :: Weighting
greedyWeighting board player = weights
    where moves = possibleMoves board player
          totalScore = foldr (\x y -> weightOf x + y) 0.0 moves
          weights = [ (pos, weight) | pos <- moves, let weight = weightOf pos * 100.0 / totalScore ]
          weightOf p = fromIntegral (score (play board player p) player)

-- 
-- minimax
-- 
-- The objective of minimax is to resolve a move from a given move state.
-- We use a depth bound depth first search to determine a move that will
-- lead to a maximum utility for the player making the move and a minimum
-- utility to the opponent.
--
-- minimax :: Weighting -> Strategy
-- minimax weighting board player = (0,0)
--
-- This is actually a wrapper for the true minimax algorithm, since the true one
-- requires a max depth to be passed into it to book keep it's state. 

minimax :: Weighting
minimax board player = weights
    where moves = possibleMoves board player
          maxDepth = 4
          weights = [ (pos, weight) | pos <- moves, let weight = getMinimax board player maxDepth pos ]

-- true minimax function
--
-- we assume that if there are no more possible moves, or if we reach max depth,
-- we return the utility values at that depth, which is the percent of the board
-- controlled. (this can be changed, its not set in stone
--
-- we assume that player 2 is maximizing and 1 is minimizing in this algorithm
--
getMinimax :: Board -> Player -> Depth -> Position -> Confidence
getMinimax prevBoard player depth position 
    | depth == 0                                                = utility
    | length $ possibleMoves $ play prevBoard player pos == 0   = utility
    | otherwise                                                 = best
    where utility = playerScore / totalPieces * 100.0
          playerScore = fromIntegral $ score newState player
          totalPieces = move newState + 4
          newState = play prevBoard player pos
          nextPlayer = if (player == 1) then 2 else 1
          newMoves = possibleMoves newState nextPlayer
          childWeights = map (getMinimax newState nextPlayer $ depth - 1) newMoves
          best = if (player == 2) then maximum childWeights else minimum childWeights

--
greedy :: Strategy
greedy board@(Board grid) player = best
    where moves = possibleMoves board player
          best = foldr (\x y -> if (better x y) then x else y) (head moves) (tail moves)
          better x y = (score (play board player x) player) > (score (play board player y) player)


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
