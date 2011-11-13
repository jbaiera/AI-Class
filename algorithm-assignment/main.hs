import Graph.Weighted as Graph
import Data.Map as Map
import Data.Set as Set

type Cost = Int
type CameFrom = Vertex
type Heuristic = (Vertex -> Cost)

cities :: [(Vertex, String, (Int, Int))]
cities = [(1, "Augusta", (35,24))
         ,(2, "Albany", (31,22))
         ,(3, "Hartford", (34, 21))
         ,(4, "Cleveland", (27,20))
         ,(5, "Dover", (33,17))
         ,(6, "Columbia", (35, 12))
         ,(7, "Lansing", (24, 21))
         ,(8, "Flint", (24, 23))
         ,(9, "Chicago", (21, 20))
         ,(10, "Atlanta", (29, 9))
         ,(11, "Tallahassee", (32, 6))
         ,(12, "Miami", (34, 3))
         ,(13, "New Orleans", (25, 6))
         ,(14, "St. Paul", (19, 22))
         ,(15, "Apostle", (22, 24))
         ,(16, "Topeka", (19, 14))
         ,(17, "Fargo", (18, 25))
         ,(18, "El Paso", (16, 6))
         ,(19, "Tucson", (11, 5))
         ,(20, "Helena", (10, 24))
         ,(21, "Boise", (9, 20))
         ,(22, "Reno", (6, 16))
         ,(23, "Los Angeles", (4, 8))
         ,(24, "San Francisco", (2, 14))
         ,(25, "Portland", (3, 21))
         ,(26, "Seattle", (3, 25))
         ,(27, "Austin", (20, 2))
         ,(28, "Santa Fe", (13, 11))
         ]

coordinates :: Vertex -> (Int, Int)
coordinates n = coords
    where (_, _, coords) = cities !! (n-1)

flights :: [(Vertex, Vertex, Weight)]
flights = [(1,2,5),(1,3,3)
          ,(2,1,5),(2,3,3),(2,4,5)
          ,(3,1,3),(3,2,3),(3,5,6)
          ,(4,2,5),(4,5,7),(4,7,3),(4,10,15)
          ,(5,3,6),(5,4,7),(5,6,9)
          ,(6,5,9),(6,10,8),(6,11,10)
          ,(7,4,3),(7,8,3),(7,9,4)
          ,(8,7,3)
          ,(9,7,4),(9,14,4),(9,16,8)
          ,(10,4,15),(10,6,8),(10,11,7),(10,13,6),(10,16,15)
          ,(11,6,10),(11,10,7),(11,13,8),(11,12,6)
          ,(12,11,6)
          ,(13,10,6),(13,11,8),(13,27,9)
          ,(14,9,4),(14,15,5),(14,17,4)
          ,(15,14,5),(15,17,5)
          ,(16,9,8),(16,10,15),(16,22,17),(16,28,8)
          ,(17,14,4),(17,15,5),(17,20,9)
          ,(18,19,7),(18,27,10),(18,28,8)
          ,(19,18,7),(19,23,11)
          ,(20,17,9),(20,21,5),(20,26,8)
          ,(21,20,5),(21,22,7)
          ,(22,16,17),(22,21,7),(22,23,11),(22,25,9),(22,28,12)
          ,(23,19,11),(23,22,11),(23,24,9)
          ,(24,23,9),(24,25,9)
          ,(25,22,9),(25,24,9),(25,26,5)
          ,(26,20,8),(26,25,5)
          ,(27,13,9),(27,18,10)
          ,(28,16,8),(28,18,8),(28,22,12)
          ]

graph :: Graph
graph = buildFromEdges flights

main = do
    print $ findPath graph 24 3

euclideanDistance :: (Int, Int) -> (Int, Int) -> Cost
euclideanDistance (x1, y1) (x2, y2) = floor . sqrt . fromIntegral $ (x1-x2)^2 + (y1-y2)^2

findPath :: Graph -> Vertex -> Vertex -> [Vertex]
findPath g from to = search g h from from to 0 cost cameFrom fringe
    where h n = euclideanDistance (coordinates to) (coordinates n)
          cost = Map.fromList [(from, 0)]
          cameFrom = Map.fromList []
          fringe = Set.fromList [from]

search :: Graph -> Heuristic -> Vertex -> Vertex -> Vertex -> Cost -> (Map Vertex Cost) -> (Map Vertex CameFrom) -> (Set Vertex) -> [Vertex]
search g h current from to distance cost cameFrom fringe
    | from == to    = reconstructPath from to cameFrom
    | otherwise     = search g h next from to distance' cost' cameFrom' fringe'
    where fringe'   = Set.fromList $ [ x | x <- (Set.toList fringe), x /= from ] ++ [ x | (x,c) <- (getNeighbors g from), Map.notMember x cost ]
          -- add the nodes to the fringe which haven't already been evaluated for costs, and remove the current node from the fringe
          -- all nodes in the fringe except the current one, and add the neighbors which do not already have costs
          cost'     = Map.fromList $ [ (x,c) | (x,y) <- (Map.toList cost), let c = min y (distance + h current) ]
          -- all the nodes in the cost map remain in it, add the neighbors which are not in the cost map or which have a lower cost (update)
          cameFrom' = Map.fromList $ [ (x,p) | (x,y) <- (Map.toList cameFrom), let p = if y <= (distance + h current) then y else current ]
          -- set the next node we go to to be coming from the current one. Replace it if it's coming from somewhere else, we're on a better path
          next      = cheapest fringe' cost'
          -- the node in the fringe with the lowest cost
          distance' = addMaybe distance (Map.lookup next cost')
          -- sets the distance from start to the next node
          cheapest :: (Set Vertex) -> (Map Vertex Cost) -> Vertex
          cheapest fringe cost = foldr (\x y -> if (Map.lookup x cost) < (Map.lookup y cost) then x else y) initial remaining
            where fringeList = Set.toList fringe
                  initial = head fringeList
                  remaining = tail fringeList

reconstructPath :: Vertex -> Vertex -> (Map Vertex CameFrom) -> [Vertex]
reconstructPath _ _ _ = []

addMaybe :: Int -> Maybe Int -> Int
addMaybe x (Just y) = x + y
addMaybe x (Nothing) = x

{-
take current node
expand it, add neighbors to fringe
for each neighbor:
        if target node:
            we have found it! we are done, just reconstruct the path
            find current node in camefrom, then its predecessor in camefrom, etc. until we have a complete path
            terminate
        if already visited (is visited if in the cost map or the cameFrom map):
            check if cheaper this route. if so, update the cost and cameFrom to reflect this
        else:
            add to cost map and cameFrom map to reflect this
-}

