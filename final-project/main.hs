import Reversi.Game

main = do
    print $ play initialBoard 1 (3,5)
    print $ possibleMoves initialBoard 1
    let curr = play initialBoard 1 (3,5)
    print $ score curr 1
    print $ score curr 2
    

