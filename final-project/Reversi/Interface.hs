module Reversi.Interface where

import Reversi.Game
import Reversi.Strategies
import Reversi.Interface.LoginInfo as LoginInfo
import System.Environment
import Control.Monad
import Database.HDBC
import Database.HDBC.MySQL
import Database.HDBC.SqlValue
import qualified Data.ByteString.Char8 as ByteString

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

{- THIS BLOCK IS FOR DATABASE CONNECTIONS
    requires: Reversi.Interface.LoginInfo with the following constants:
                dbHostname
                dbUsername
                dbPassword
                dbDatabase
                dbSocket
-}

type GameId = Int

conn = connectMySQL defaultMySQLConnectInfo {
            mysqlHost       = LoginInfo.dbHostname,
            mysqlUser       = LoginInfo.dbUsername,
            mysqlPassword   = LoginInfo.dbPassword,
            mysqlDatabase   = LoginInfo.dbDatabase,
            mysqlUnixSocket = LoginInfo.dbSocket
        }

-- Takes a game, player, and position and tries to update the database for that move
commitMove :: Connection -> GameId -> Player -> Position -> IO ()
commitMove conn game player position = do
    let opponent = getOpponent player
    let fetchQuery = "SELECT board_state FROM games WHERE game_id = ?"
    let updateQuery = "UPDATE games SET board_state = ?, to_move = ? WHERE game_id = ?"
    fetchResults <- quickQuery' conn fetchQuery [toSql game]
    let board = Board (read (sqlToString $ fetchResults !! 0 !! 0) :: [[Int]])
    let (Board grid') = play board player position
    updateResults <- quickQuery' conn updateQuery [SqlString (show grid'), SqlInteger (fromIntegral opponent), SqlInteger (fromIntegral game)]
    return ()

-- Takes a game, player, and strategy and tries to update the database for that move
aiCommitMove :: Connection -> GameId -> Player -> Strategy -> IO ()
aiCommitMove conn game player strategy = do
    let opponent = getOpponent player
    let fetchQuery = "SELECT board_state FROM games WHERE game_id = ?"
    fetchResults <- quickQuery' conn fetchQuery [toSql game]
    let board = Board (read (sqlToString $ fetchResults !! 0 !! 0) :: [[Int]])
    let pass = ((length $ possibleMoves board player) == 0)
    let gameover = pass && ((length $ possibleMoves board opponent) == 0)
    let (Board grid') = if pass then board else play board player (strategy board player)
    let winner = if (score board 1) > (score board 2) then 1 else 2
    let updateQuery = if not gameover
                        then "UPDATE games SET board_state = ?, to_move = ? WHERE game_id = ?"
                        else "UPDATE games SET winner = ?"
    updateResults <- if not gameover
                        then quickQuery' conn updateQuery [SqlString (show grid'), SqlInteger (fromIntegral opponent), SqlInteger (fromIntegral game)]
                        else quickQuery' conn updateQuery [SqlInteger winner]
    return ()


-- Get valid moves for a player in a game
getAllValid :: Connection -> GameId -> Player -> IO [Position]
getAllValid conn game player = do
    results <- quickQuery' conn "SELECT board_state FROM games WHERE game_id = ?" [toSql game]
    let board = Board (read (sqlToString $ results !! 0 !! 0) :: [[Int]])
    return $ possibleMoves board player

sqlToString (SqlByteString bs) = ByteString.unpack bs
stringToSql str = ByteString.pack str

