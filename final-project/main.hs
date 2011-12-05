import Reversi.Game
import Reversi.Strategies
import Reversi.Interface
import System.Random

main = do
    {-
    putStr "Greedy vs greedy:           "
    shortSimulate initialBoard 1 greedy greedy
    putStr "Greedy vs evaporation:      "
    shortSimulate initialBoard 1 greedy evaporation
    putStr "Greedy vs mobility:         "
    shortSimulate initialBoard 1 greedy mobility
    putStr "Evaporation vs greedy:      "
    shortSimulate initialBoard 1 evaporation greedy
    putStr "Evaporation vs evaporation: "
    shortSimulate initialBoard 1 evaporation evaporation
    putStr "Evaporation vs mobility:    "
    shortSimulate initialBoard 1 evaporation mobility
    putStr "Mobility vs greedy:         "
    shortSimulate initialBoard 1 mobility greedy
    putStr "Mobility vs evaporation:    "
    shortSimulate initialBoard 1 mobility evaporation
    putStr "Mobility vs mobility:       "
    shortSimulate initialBoard 1 mobility mobility
    -}
    {-
    let strategy = randomAlphabeta (mkStdGen 1324) 6 greedyEval
    let strategies = [ randomAlphabeta (mkStdGen i) 6 greedyEval | i <- [1..100] ]
    --printAndRun initialBoard 1 strategy greedy
    --mapM_ (\s -> print $ simulate initialBoard 1 s greedy) strategies
    let results = foldr (\s (w1,w2) -> case (simulate initialBoard 1 s greedy) of
                                            0 -> (w1, w2)
                                            1 -> (w1+1, w2)
                                            2 -> (w1, w2+1)) (0,0) strategies
    print results
    -}
    printAndRun initialBoard 1 (randomAlphabeta (mkStdGen 1324) 6 heatmapEval) greedy

