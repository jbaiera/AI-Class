import Reversi.Game
import Reversi.Strategies

main = do
    prettyPrint $ play initialBoard 1 (3,5)
    print $ possibleMoves initialBoard 1
    let curr = play initialBoard 1 (3,5)
    prettyPrint curr
    print $ score curr 1
    print $ score curr 2
    print $ greedy curr 2
    prettyPrint $ play curr 2 $ greedy curr 2
    

