import Reversi.Game
import Reversi.Strategies
import Reversi.Interface
import System.Random

main = do
    let strategy = randomAlphabeta (mkStdGen 1324) 4 greedyEval
    let strategies = [ randomAlphabeta (mkStdGen i) 6 heatmapEval | i <- [1..10] ]
    let strategies' = [ randomAlphabeta (mkStdGen i) 4 greedyEval | i <- [10,9..1] ]
    let pairs = zip strategies strategies'
    --printAndRun initialBoard 1 strategy greedy
    --mapM_ (\s -> print $ simulate initialBoard 1 s greedy) strategies
    let results = map (\(s1,s2) -> simulate initialBoard 1 s1 s2) pairs
    let sum = foldr (\w (w1,w2) -> case w of
                                    0 -> (w1, w2)
                                    1 -> (w1+1, w2)
                                    2 -> (w1, w2+1)) (0,0) results
    print sum
    {-
    let results = foldr (\(s1,s2) (w1,w2) -> case (simulate initialBoard 1 s1 s2) of
                                            0 -> (w1, w2)
                                            1 -> (w1+1, w2)
                                            2 -> (w1, w2+1)) (0,0) pairs
    print results
    let results2 = foldr (\(s1,s2) (w1,w2) -> case (simulate initialBoard 1 s2 s1) of
                                            0 -> (w1, w2)
                                            1 -> (w1+1, w2)
                                            2 -> (w1, w2+1)) (0,0) pairs
    print results2
    -}
    --printAndRun initialBoard 1 (randomAlphabeta (mkStdGen 1720) 6 heatmapEval) greedy

