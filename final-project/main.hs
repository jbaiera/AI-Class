import Reversi.Game

main = do
    print initialBoard
    print $ play initialBoard 1 (3,5)
    print $ set initialGrid (3,2) 1
    print $ playable initialBoard 1 (3,5)
    

