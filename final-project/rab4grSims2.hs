import Reversi.Game
import Reversi.Strategies
import Reversi.Interface
import System.Random

roundNums = [501..1000]

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

summarize :: [(GameOverState, Int, Int)] -> String -> String -> IO ()
summarize results black white = do
    putStrLn $ black ++ " won " ++ (show bwins) ++ " times."
    putStrLn $ white ++ " won " ++ (show wwins) ++ " times."
    putStrLn $ "There were " ++ (show ties) ++ " ties."
    putStrLn $ black ++ " scored " ++ (show bavg) ++ " on average."
    putStrLn $ white ++ " scored " ++ (show wavg) ++ " on average."
    putStrLn $ ""
    where wwins = foldr (\(x,_,_) n -> if x == 2 then n+1 else n) 0 results
          bwins = foldr (\(x,_,_) n -> if x == 1 then n+1 else n) 0 results
          ties  = foldr (\(x,_,_) n -> if x == 0 then n+1 else n) 0 results
          wavg  = (fromIntegral $ foldr (\(_,_,x) n -> x+n) 0 results) / (fromIntegral $ length results)
          bavg  = (fromIntegral $ foldr (\(_,x,_) n -> x+n) 0 results) / (fromIntegral $ length results)

main = do
    putStrLn "rab4gr vs. greedy:"
    let greedyResults = doSimulation rab4grList greedyList
    print greedyResults
    summarize greedyResults "rab4gr" "greedy"
    --
    putStrLn "rab4gr vs. rab4hm:"
    let rab4hmResults = doSimulation rab4grList rab4hmList
    print rab4hmResults
    summarize rab4hmResults "rab4gr" "rab4hm"
    --
    putStrLn "rab4gr vs. rab6hm:"
    let rab6hmResults = doSimulation rab4grList rab6hmList
    print rab6hmResults
    summarize rab6hmResults "rab4gr" "rab6hm"
    --
    putStrLn "rab4gr vs. rab4gr:"
    let rab4grResults = doSimulation rab4grList rab4grList
    print rab4grResults
    summarize rab4grResults "rab4gr" "rab4gr"
    --
    return ()


