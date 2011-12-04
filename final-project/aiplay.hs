import Reversi.Interface
import Reversi.Game
import Reversi.Strategies
import System.Environment

main = do
    args <- getArgs
    let strategyId = read (args !! 0) :: Int
    let player = read (args !! 1) :: Int
    let gameId = read (args !! 2) :: Int
    let strategy = case (strategyId) of
                    1 -> greedy
                    2 -> minimax4
                    3 -> minimax8
    connection <- conn
    aiCommitMove connection gameId player strategy

