module Reversi.Game where

-- the board is a square
boardSize :: Int
boardSize = 8

type Player = Int

data Board = Board [[Player]] deriving (Show, Read, Eq)

--play :: Board -> (Int, Int) -> Board

