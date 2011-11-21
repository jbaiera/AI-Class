import Reversi.Game

main = do
    print $ play initialBoard 1 (3,5)
    print $ possibleMoves initialBoard 1
    

