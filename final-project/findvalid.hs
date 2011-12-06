import Reversi.Game
import Reversi.SqlInterface
import Reversi.Strategies
import Reversi.Interface
import System.Environment

main = do
    args <- getArgs
    let game = read (args !! 0) :: GameId
    let player = read (args !! 1) :: Player
    connection <- conn
    moves <- getAllValid connection game player
    print moves

