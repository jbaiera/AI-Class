import Reversi.Interface
import Reversi.Game
import Reversi.Strategies
import System.Environment

main = do
    args <- getArgs
    let strategyId = read (args !! 0) :: Int
    let player = read (args !! 1) :: Int
    let gameId = read (args !! 2) :: Int
    let strategy = if strategyId == 1 then greedy else greedy
    connection <- conn
    aiCommitMove connection gameId player strategy

