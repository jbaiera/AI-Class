module Reversi.Strategies where

import Reversi.Game

type Strategy = Board -> Player -> Position

-- Confidence should be in the range of 0.0 to 100.0.
type Confidence = Float
type Depth = Int

type Weighting = Board -> Player -> [(Position, Confidence)]

type Heuristic = Board -> Player -> Position -> Int

greedyWeighting :: Weighting
greedyWeighting board player = weights
    where moves = possibleMoves board player
          totalScore = foldr (\x y -> weightOf x + y) 0.0 moves
          weights = [ (pos, weight) | pos <- moves, let weight = weightOf pos * 100.0 / totalScore ]
          weightOf p = fromIntegral (score (play board player p) player)

greedy :: Strategy
greedy board@(Board grid) player = best
    where moves = possibleMoves board player
          best = foldr (\x y -> if (better x y) then x else y) (head moves) (tail moves)
          better x y = (score (play board player x) player) > (score (play board player y) player)

greedyEval :: Heuristic
greedyEval board player position = score (play board player position) player

minimax :: Depth -> Heuristic -> Strategy
minimax depth heuristic board player = fst $ minimax' depth heuristic player board player

minimax' :: Depth -> Heuristic -> Player -> Board -> Player -> (Position, Int)
minimax' depth heuristicFunction maxPlayer board currPlayer = result
    where heuristic = heuristicFunction board maxPlayer
          moves = possibleMoves board currPlayer
          boards = map (\m -> (m, play board currPlayer m)) moves
          next = getOpponent currPlayer
          children = map (\(m, b) -> (m, snd $ minimax' (depth-1) heuristicFunction maxPlayer b (getOpponent next))) boards
          comparator | currPlayer == maxPlayer  = (>)
                     | otherwise                = (<)
          choose = (\x y -> if (heuristic x `comparator` heuristic y) then x else y)
          bestMove = foldr choose (head moves) (tail moves)
          pickBest = (\(m1,s1) (m2,s2) -> if (s1 `comparator` s2) then (m1,s1) else (m2,s2))
          result | depth == 0   = (bestMove, heuristic bestMove)
                 | otherwise    = foldr pickBest (head children) (tail children)

{-
negamax :: Depth -> Strategy
negamax depth board player = negamaxResults
    where opponent = getOpponent player
          moves = possibleMoves board player
          boards = map (\m -> (m, play board player m)) moves
          children = map (\(m,b) -> (m, negamax (depth-1) b opponent)) boards
          heuristic = greedyEval board player
          bestMove = foldr (\x y -> if (heuristic x > heuristic y) then x else y) (head moves) (tail moves)
          negamaxResults | depth == 0   = bestMove
                         | otherwise    = foldr (\(m,p) -> 
-}
{-
minWeight :: [(Position, Confidence)] -> (Position, Confidence)
minWeight = swap . minimum . (map swap)

maxWeight :: [(Position, Confidence)] -> (Position, Confidence)
maxWeight = swap . maximum . (map swap)

swap :: (a, b) -> (b, a)
swap (x,y) = (y,x)
-}

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
