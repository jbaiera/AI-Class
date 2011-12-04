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
initialGrid :: [[Int]]
initialGrid = [[0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0],
               [0,0,0,1,2,0,0,0],
               [0,0,0,2,1,0,0,0],
               [0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0]]

initialBoard :: Board
initialBoard = Board initialGrid

-- makes it pretty
prettyPrint :: Board -> IO ()
prettyPrint (Board grid) = do putStrLn "Current score..."
                              putStrLn $ " Player 1: " ++ (show $ score board 1)
                              putStrLn $ " Player 2: " ++ (show $ score board 2)
                              mapM_ (putStrLn . toStr) grid
                              putStrLn ""
                              where toStr = map transchar
                                    transchar 0 = '.'
                                    transchar 1 = 'O'
                                    transchar 2 = '@'
                                    board = Board grid

-- gets the opponent of the current player
getOpponent :: Player -> Player
getOpponent p = 3 - p

-- prints the winner
printWinner :: Board -> IO ()
printWinner board = putStrLn status
    where status
              | score board 1 > score board 2 = "Player 1 wins!"
              | score board 2 > score board 1 = "Player 2 wins!"
              | otherwise = "Draw!"

-- returns the score for that player
score :: Board -> Player -> Int
score (Board grid) player = foldr (\r n -> n + scoreRow r) 0 grid
    where scoreRow = foldr (\x n -> if x == player then 1+n else n) 0

-- returns what move it is
move :: Board -> Int
move board = score board 1 + score board 2 - 4

-- gives all the possible moves for a certain position
possibleMoves :: Board -> Player -> [Position]
possibleMoves board player
    | length positions == 0     = [pass]
    | otherwise                 = positions
    where positions = [ (x,y) | x <- [0..7], y <- [0..7], playable board player (x,y) ]

pass :: Position
pass = ((-1),(-1))

-- play takes a player and position and execute the move
play :: Board -> Player -> Position -> Board
play board player position = board'
    where directions = [ (a,b) | a <- [(-1),0,1], b <- [(-1),0,1], (a,b) /= (0,0) ]
          board' | position == pass = board
                 | otherwise        = setPos position $ foldr (\d b -> unsetPos position $ capture b player position d) board directions
          unsetPos position (Board grid) = Board (set grid position 0)
          setPos position (Board grid) = Board (set grid position player)


-- playable checks if a move "makes sense" to make
playable :: Board -> Player -> Position -> Bool
playable board@(Board grid) player position@(px,py) = current && anyValid
    where dirs = [ (a,b) | a <- [(-1),0,1], b <- [(-1),0,1], (a,b) /= (0,0) ]
          anyValid = foldr (\d s -> s || valid board player position d) False dirs
          current = (grid !! px !! py) == 0


-- valid determines whether or not we should flip tiles in that direction
valid :: Board -> Player -> Position -> Direction -> Bool
valid board@(Board grid) player position@(px, py) direction@(dx, dy)
    | outOfBounds (px+dx, py+dy)                = False
    | current == opponent && next == player     = True
    | next == 0                                 = False
    | current == player                         = False
    | otherwise                                 = valid board player (px+dx, py+dy) direction
    where current = grid !! px !! py
          next = grid !! (px+dx) !! (py+dy)
          opponent = case player of
                1 -> 2
                2 -> 1

-- capture will flip all the tiles in one direction
capture :: Board -> Player -> Position -> Direction -> Board
capture board@(Board grid) player position@(px, py) direction@(dx, dy)
    | not flipping  = (Board grid)
    | flipping      = capture (Board grid')  player (px+dx, py+dy) direction
    where flipping = valid board player position direction
          grid' = set grid position player

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

