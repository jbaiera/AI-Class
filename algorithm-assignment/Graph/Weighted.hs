module Graph.Weighted (Graph, Edge, Vertex, Weight, buildFromEdges, getNeighbors) where

data Graph = Graph [(Vertex, [(Vertex, Weight)])]   deriving (Show, Read)
data Edge = Edge Vertex Vertex Weight               deriving (Show, Read)
data Bounds = Bounds Vertex Vertex                  deriving (Show, Read)
type Vertex = Int
type Weight = Int

makeGraph :: Bounds -> Graph
makeGraph (Bounds low high) = Graph (map (\x -> (x, [])) [low..high])

getNeighbors :: Graph -> Vertex -> [(Vertex, Weight)]
getNeighbors (Graph e) v = neighbors
    where (first, _) = e !! 0
          (_, neighbors) = e !! (v - first)

buildFromEdges :: [(Vertex, Vertex, Weight)] -> Graph
buildFromEdges [] = Graph []
buildFromEdges xs = addEdges xs $ makeGraph $ findBounds xs

addEdges :: [(Vertex, Vertex, Weight)] -> Graph -> Graph
addEdges [] g = g
addEdges (e:es) g = addEdges es $ addEdge e g

addEdge :: (Vertex, Vertex, Weight) -> Graph -> Graph
addEdge (from, to, cost) (Graph vertices) = Graph newvertices
    where (first, _) = vertices !! 0
          index = from - first
          (_, fromEdges) = vertices !! index
          newvertices = replaceEdge (from, (insertNeighbor fromEdges (to, cost))) vertices

replaceEdge :: (Vertex, [(Vertex, Weight)]) -> [(Vertex, [(Vertex, Weight)])] -> [(Vertex, [(Vertex, Weight)])] 
replaceEdge (k, e) ((ok, oe):os)
    | k == ok   = (k, e) : os
    | otherwise = (ok, oe) : replaceEdge (k, e) os

insertNeighbor :: [(Vertex, Weight)] -> (Vertex, Weight)-> [(Vertex, Weight)]
insertNeighbor [] x = [x]
insertNeighbor ((v,w):ns) (nv, nw)
    | nv < v    = (nv, nw) : (v,w) : ns
    | otherwise = (v, w) : insert ns (nv, nw)

insert :: (Ord a) => [a] -> a -> [a]
insert [] a = [a]
insert (x:xs) a
    | x < a     = x : insert xs a
    | otherwise = a : x : xs

findBounds :: [(Vertex, Vertex, Weight)] -> Bounds
findBounds [] = Bounds 0 0
findBounds (x:xs) = Bounds lowest highest
    where (from, to, _) = x
          (low, high) = if from < to then (from, to) else (to, from)
          (Bounds nextlow nexthigh) = findBounds xs
          lowest = if low < nextlow then low else nextlow
          highest = if high > nexthigh then high else nexthigh

