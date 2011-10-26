import Graph.Weighted

graph = buildFromEdges [(2,1,4),(1,3,1),(3,2,3),(1,4,2),(4,3,5),(3,4,3)]


main = do
    print graph
    print $ getNeighbors graph 3

