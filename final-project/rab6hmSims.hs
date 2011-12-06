import Reversi.Game
import Reversi.Strategies
import Reversi.Interface
import System.Random

roundNums = [1..1000]

greedySeed = 123421
rab4hmSeed = 135754
rab6hmSeed = 583862
rab4grSeed = 791215

greedyList = [ greedy | i <- roundNums ]
rab4hmList = [ randomAlphabeta (mkStdGen (i+rab4hmSeed)) 4 heatmapEval | i <- roundNums ]
rab6hmList = [ randomAlphabeta (mkStdGen (i+rab6hmSeed)) 6 heatmapEval | i <- roundNums ]
rab4grList = [ randomAlphabeta (mkStdGen (i+rab4grSeed)) 4 greedyEval  | i <- roundNums ]

strategies = [ greedyList, rab4hmList, rab6hmList, rab4grList ]

bStrategies = strategies
wStrategies = strategies

pairings :: [ ([Strategy], [Strategy]) ]
pairings = [ (b, w) | b <- bStrategies, w <- wStrategies ]

main = do
    putStrLn "win."
    


