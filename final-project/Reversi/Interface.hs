module Reversi.Interface where

import Reversi.Game
import Reversi.Strategies
import Reversi.Interface.LoginInfo as LoginInfo
import System.Environment
import Control.Monad
import Database.HDBC
import Database.HDBC.MySQL

-- plays a game between two functions, printing the state of the board each time
simulate :: Board -> Player -> Strategy -> Strategy -> IO ()
simulate board player strategy1 strategy2
    | possibleMoves board player == []  = do prettyPrint board
                                             printWinner board
    | player == 1  = do prettyPrint board
                        simulate (play board 1 $ strategy1 board player) 2 strategy1 strategy2
    | otherwise    = do prettyPrint board
                        simulate (play board 2 $ strategy2 board player) 1 strategy1 strategy2

-- plays a game between two functions, only printing the final status
shortSimulate :: Board -> Player -> Strategy -> Strategy -> IO ()
shortSimulate board player strategy1 strategy2
    | possibleMoves board player == []  = printWinner board
    | player == 1  = shortSimulate (play board 1 $ strategy1 board player) 2 strategy1 strategy2
    | otherwise    = shortSimulate (play board 2 $ strategy2 board player) 1 strategy1 strategy2

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


{- THIS BLOCK IS FOR DATABASE CONNECTIONS
    requires: Reversi.Interface.LoginInfo with the following constants:
                dbHostname
                dbUsername
                dbPassword
                dbDatabase
                dbSocket
-}

type GameId = Int

conn :: IO Connection
conn = connectMySQL defaultMySQLConnectInfo {
            mysqlHost       = LoginInfo.dbHostname
            mysqlUser       = LoginInfo.dbUsername
            mysqlPassword   = LoginInfo.dbPassword
            mysqlDatabase   = LoginInfo.dbDatabase
            mysqlUnixSocket = LoginInfo.dbSocket
        }

-- Takes a game, player, and position and tries to update the database for that move
commitMove :: GameId -> Player -> Position -> IO Connection -> IO ()
commitMove = return ()

