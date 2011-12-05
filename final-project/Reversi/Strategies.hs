module Reversi.Strategies where

import Reversi.Game
import System.Random

-- Types ======================================================================

type Strategy = Board -> Player -> Position

-- Confidence should be in the range of 0.0 to 100.0.
type Confidence = Float
type Depth = Int

type Weighting = Board -> Player -> [(Position, Confidence)]

type Heuristic = Board -> Player -> Position -> Int

type Heatmap = [[Int]]

-- Weighting ==================================================================

greedyWeighting :: Weighting
greedyWeighting board player = weights
    where moves = possibleMoves board player
          totalScore = foldr (\x y -> weightOf x + y) 0.0 moves
          weights = [ (pos, weight) | pos <- moves, let weight = weightOf pos * 100.0 / totalScore ]
          weightOf p = fromIntegral (score (play board player p) player)

-- Strategies =================================================================

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

alphabeta1 :: Strategy
alphabeta1 = alphabeta 6 greedyEval

-- Heuristics =================================================================

greedyEval :: Heuristic
greedyEval board player position = score (play board player position) player

heatmapEval :: Heuristic
heatmapEval board player position
    | position == pass  = rawEval
    | posRegion == 5    = rawEval + 30
    | otherwise         = rawEval
    {-
    | posRegion == 5    = 20 + rawEval                              -- TAKE IT
    | posRegion == 4    = rawEval - 20                               -- RUN AWAY
    | posRegion == 3    = rawEval + 10     -- Less than perfect 
    | posRegion == 2    = rawEval - 10     -- RUN slightly less
    | otherwise         = rawEval                                   -- Whatever
    -}
    where posRegion = (regionmap !! (fst position)) !! (snd position)
          rawEval   = greedyEval board player position
          regionmap = [[5,4,3,3,3,3,4,5],
                       [4,4,2,2,2,2,4,4],
                       [3,2,1,1,1,1,2,3],
                       [3,2,1,1,1,1,2,3],
                       [3,2,1,1,1,1,2,3],
                       [3,2,1,1,1,1,2,3],
                       [4,4,2,2,2,2,4,4],
                       [5,4,3,3,3,3,4,5]]

-- Complex Strategies =========================================================

minimax :: Depth -> Heuristic -> Strategy
minimax depth heuristic board player = bestMove
    where moves = possibleMoves board player
          scores = [ (score, position) | position <- moves, let score = minimax' depth heuristic player (play board player position) player ]
          maxScore = fst $ maximum scores
          bestMoves = [ p | (s,p) <- scores, s == maxScore ]
          bestMove = head bestMoves

randomMinimax :: (RandomGen g) => g -> Depth -> Heuristic -> Strategy
randomMinimax randomGen depth heuristic board player = bestMove
    where moves = possibleMoves board player
          scores = [ (score, position) | position <- moves, let score = minimax' depth heuristic player (play board player position) player ]
          maxScore = fst $ maximum scores
          bestMoves = [ p | (s,p) <- scores, s == maxScore ]
          bestMove = bestMoves !! (fst (random randomGen) `mod` (length bestMoves))

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

randomAlphabeta :: (RandomGen g) => g-> Depth -> Heuristic -> Strategy
randomAlphabeta randomGen depth heuristic board player = bestMove
    where moves = possibleMoves board player
          scores = [ (val, position) | position <- moves, let val = alphabeta' depth heuristic player (play board player position) player 10000 0 ]
          maxScore = fst $ maximum scores
          bestMoves = [ p | (s,p) <- scores, s == maxScore ]
          bestMove = bestMoves !! (fst (random randomGen) `mod` (length bestMoves))

alphabeta :: Depth -> Heuristic -> Strategy
alphabeta depth heuristic board player = bestMove
    where moves = possibleMoves board player
          scores = [ (val, position) | position <- moves, let val = alphabeta' depth heuristic player (play board player position) player 10000 0 ]
          bestMove = snd $ maximum scores

--            depth    heuristic    maxPlayer board    current   alpha  beta   result 
alphabeta' :: Depth -> Heuristic -> Player -> Board -> Player -> Int -> Int -> Int
alphabeta' depth heuristicFunction maxPlayer board currPlayer alpha beta
    | depth == 0    = heuristicFunction board maxPlayer pass
    | otherwise     = result
    where moves = possibleMoves board currPlayer
          maxing = (currPlayer == maxPlayer)
          children = map (\m -> play board currPlayer m) moves
          result | maxing       = alpha'
                 | otherwise    = beta'
          alpha' = cfold (\a -> beta <= a)  (\m a -> max a $ recurse m a beta)  (recurse (head moves) alpha beta) (tail moves)
          beta'  = cfold (\b -> alpha <= b) (\m b -> min b $ recurse m alpha b) (recurse (head moves) alpha beta) (tail moves)
          nextPlayer = getOpponent currPlayer
          recurse m a b = alphabeta' (depth-1) heuristicFunction maxPlayer (play board currPlayer m) nextPlayer a b

-- cfold takes a cutoff criteria, too
cfold :: (b -> Bool) -> (a -> b -> b) -> b -> [a] -> b
cfold _ _ y [] = y
cfold cutoff f y (x:xs)
    | cutoff y  = y
    | otherwise = cfold cutoff f (f x y) xs


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
