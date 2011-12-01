import Reversi.Interface
import Reversi.Game
import System.Environment

main = do
    args <- getArgs
    let pos = read (args !! 0) :: Position
    let player = read (args !! 1) :: Int
    let gameId = read (args !! 2) :: Int
    connection <- conn
    commitMove connection gameId player pos

