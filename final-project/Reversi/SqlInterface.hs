module Reversi.SqlInterface where

import Reversi.Game
import Reversi.Interface
import Reversi.Strategies
import Reversi.Interface.LoginInfo as LoginInfo
import System.Environment
import Control.Monad
import Database.HDBC
import Database.HDBC.MySQL
import Database.HDBC.SqlValue
import qualified Data.ByteString.Char8 as ByteString

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

