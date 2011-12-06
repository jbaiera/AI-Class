import Reversi.Game
import Reversi.Strategies
import Reversi.Interface
import System.Random

roundNums = [1..1000] -- split this into parts 1 and 2, 1..500 and 501..1000

greedySeed = 123421
rab4hmSeed = 135754
rab6hmSeed = 583862
rab4grSeed = 791215

greedyList = [ greedy | i <- roundNums ]
rab4hmList = [ randomAlphabeta (mkStdGen (i+rab4hmSeed)) 4 heatmapEval | i <- roundNums ]
rab6hmList = [ randomAlphabeta (mkStdGen (i+rab6hmSeed)) 6 heatmapEval | i <- roundNums ]
rab4grList = [ randomAlphabeta (mkStdGen (i+rab4grSeed)) 4 greedyEval  | i <- roundNums ]

doSimulation :: [Strategy] -> [Strategy] -> [(GameOverState, Int, Int)]
doSimulation xs ys = results
    where pairs = zip xs ys
          results = map (\(x,y) -> simulateScore initialBoard 1 x y) pairs

main = do
    putStrLn "greedy vs. greedy:"
    let greedyResults = doSimulation greedyList greedyList
    print greedyResults
    --
    putStrLn "greedy vs. rab4hm:"
    let rab4hmResults = doSimulation greedyList rab4hmList
    print rab4hmResults
    --
    putStrLn "greedy vs. rab6hm:"
    let rab6hmResults = doSimulation greedyList rab6hmList
    print rab6hmResults
    --
    putStrLn "greedy vs. rab4gr:"
    let rab4grResults = doSimulation greedyList rab4grList
    print rab4grResults
    --
    return ()


