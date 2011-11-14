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

-- the default starting position
initialBoard :: Board
initialBoard = Board [[0,0,0,0,0,0,0,0],
                      [0,0,0,0,0,0,0,0],
                      [0,0,0,0,0,0,0,0],
                      [0,0,0,1,2,0,0,0],
                      [0,0,0,2,1,0,0,0],
                      [0,0,0,0,0,0,0,0],
                      [0,0,0,0,0,0,0,0],
                      [0,0,0,0,0,0,0,0]]

--play :: Board -> Player -> Position -> Board
play board player position = board'
    where directions = [ (a,b) | a <- [(-1),0,1], b <- [(-1),0,1],
                                 valid board player position (a,b) ]
          board' = foldr (\d b -> capture b player position d) board directions

-- valid determines whether or not we should flip tiles in that direction
valid :: Board -> Player -> Position -> Direction -> Bool
valid board@(Board grid) player position@(px, py) direction@(dx, dy)
    | outOfBounds (px + dx, py + dy)               = False
    | current == player || current == 0            = True
    | otherwise                                    = valid board player (px+dx,py+dy) direction
    where current = grid !! (px+dx) !! (py+dy)

-- capture will flip all the tiles in one direction
capture :: Board -> Player -> Position -> Direction -> Board
capture board@(Board grid) player position@(px, py) direction@(dx, dy)
    | outOfBounds (px + dx, py + dy) = board
    | current == player || current == 0 = board
    | otherwise = capture (Board grid') player (px + dx, py + dy) direction
    where grid' = set grid position player
          current = grid !! (px+dx) !! (py+dy)

-- outOfBounds checks if a position is a valid board position
outOfBounds :: Position -> Bool
outOfBounds (x, y) = not $ 0 <= x && x < 8 && 0 <= y && y < 8

-- set sets an element of the grid for a board
set :: [[Int]] -> Position -> Int -> [[Int]]
set matrix position@(x,y) n = start ++ middle ++ end
    where start  = take x matrix
          end    = drop (x+1) matrix
          middle = [(take y row) ++ [n] ++ (drop (y+1) row)]
          row = head $ drop x matrix

