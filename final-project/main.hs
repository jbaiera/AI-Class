import Reversi.Game

main = do
    print initialBoard
    print $ play initialBoard 1 (3,5)
    print $ set initialGrid (3,2) 1
    --print $ map (\d -> valid initialBoard 1 (2,3) d) [(x,y) | x <- [(-1)..1], y <- [(-1)..1] ]

