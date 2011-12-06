module Reversi.Interface where

import Reversi.Game
import Reversi.Strategies
import System.Environment
import Control.Monad

-- plays a game between two functions, printing the state of the board each time
printAndRun :: Board -> Player -> Strategy -> Strategy -> IO ()
printAndRun board player strategy1 strategy2
    | bothPass = do prettyPrint board
                    printWinner board
    | player == 1  = do prettyPrint board
                        printAndRun (play board 1 $ strategy1 board player) 2 strategy1 strategy2
    | otherwise    = do prettyPrint board
                        printAndRun (play board 2 $ strategy2 board player) 1 strategy1 strategy2
    where bothPass = ((possibleMoves board player) == [pass]) && ((possibleMoves board (getOpponent player)) == [pass])

-- plays a game between two strategies and returns the game state
simulateScore :: Board -> Player -> Strategy -> Strategy -> (GameOverState, Int, Int)
simulateScore board player strategy1 strategy2
    | bothPass = (getWinner board, score board 1, score board 2)
    | player == 1  = simulateScore (play board 1 $ strategy1 board player) 2 strategy1 strategy2
    | otherwise    = simulateScore (play board 2 $ strategy2 board player) 1 strategy1 strategy2
    where bothPass = ((possibleMoves board player) == [pass]) && ((possibleMoves board (getOpponent player)) == [pass])


-- plays a game between two strategies and returns the game state
simulate :: Board -> Player -> Strategy -> Strategy -> GameOverState
simulate board player strategy1 strategy2
    | bothPass = getWinner board
    | player == 1  = simulate (play board 1 $ strategy1 board player) 2 strategy1 strategy2
    | otherwise    = simulate (play board 2 $ strategy2 board player) 1 strategy1 strategy2
    where bothPass = ((possibleMoves board player) == [pass]) && ((possibleMoves board (getOpponent player)) == [pass])

-- reads a board, a move, and a player from the command line, then prints the moved board
readAndPlay :: IO ()
readAndPlay = do
    args <- getArgs
    let grid = read (args !! 0) :: [[Int]]
    let position = read (args !! 1) :: Position
    let player = read (args !! 2) :: Player
    let (Board grid') = play (Board grid) player position
    putStrLn $ show grid'

-- reads a board, a strategy, and a player from the command line, then prints the moved board
readPlayStrategy :: IO ()
readPlayStrategy = do
    args <- getArgs
    let grid = read (args !! 0) :: [[Int]]
    let strategyDescriptor = read (args !! 1) :: String
    let player = read (args !! 2) :: Player
    let strategy = greedy -- generalize this... obviously...
    let (Board grid') = play (Board grid) player (strategy (Board grid) player)
    putStrLn $ show grid'

-- prints the winner
printWinner :: Board -> IO ()
printWinner board = putStrLn status
    where status | winner == 1  = "Player 1 wins!"
                 | winner == 2  = "Player 2 wins!"
                 | otherwise    = "It's a draw!"
          winner = getWinner board

