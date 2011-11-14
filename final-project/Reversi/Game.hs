module Reversi.Game where

-- the board is a square
boardSize :: Int
boardSize = 8

type Player = Int
-- choices: 0 for unmarked
--          1 for player 1
--          2 for player 2

type Direction = (Int, Int)
type Position = (Int, Int)

data Board = Board [[Player]] deriving (Show, Read, Eq)

--play :: Board -> Player -> Position -> Board
--play board player position = board'
--    where directions = [ (a,b) | a <- [(-1),0,1], b <- [(-1),0,1],
--                         flippable board player position (a,b) ]
--          board' = something

-- flippable determines whether or not we should flip tiles in that direction
--flippable :: Board -> Player -> Position -> Direction -> Bool

-- flip will flip all the tiles in one direction
--flip :: Board -> Player -> Position -> Direction -> Board

