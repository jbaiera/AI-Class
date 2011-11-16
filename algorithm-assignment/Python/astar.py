#########
# astar #
################################################################################
# James Baiera                                                                 #
# Kent State University                                                        #
# 11-13-2011                                                                   #
#                                                                              #
# A* is a searching and graph traversal algorithm used in artificial           #
#   intellegence. It uses a search heuristic to find the best path from a      #
#   starting state to a goal state.                                            #
# There are a set of helper functions required for A*. The first is the funcn  #
#   that represents the heuristic. For this A* search, our heuristic is the    #
#   euclidean distance between two points on the graph. The other function we  #
#   must have is a path construction algorithm, which returns the path from    #
#   the traversed options. This way, once we find our goal, we can trace our   #
#   steps back to the starting state.                                          #
# A* must perform a complex set of important functionalities and keep track of #
#   a set of data. For Python, A* is best to be coded in a fashion that is     #
#   iterative.                                                                 #
# A* must take the current node, and add it's neighbors to the fringe. If the  #
#   neighbors are not already in there, they are updated with where they came  #
#   from as well and their heuristics are added. If they were in there         #
#   already, they are reevaluated for traversal. After this is all finished,   #
#   the algorithm iterates once more, this time with the next most efficient   #
#   step.                                                                      #
################################################################################

import city
import math

def getEuclideanDistance(s=(0,0), g=(0,0)):
    """getEuclideanDistance :: (num, num) -> (num, num) -> num"""
    x1, y1 = s
    x2, y2 = g
    dx = math.fabs(x1 - x2)
    dy = math.fabs(y1 - y2)
    #  c^2 =             a ^ 2   +  b  ^ 2
    #  c = sqrt(c^2)
    return math.sqrt((dx ** 2) + (dy ** 2))

def reconstructPath(prev={} , current=0):
    """reconstructPath :: {nodenumber : prevnode} -> nodenumber -> [nodenumbers]"""
    if current in prev.keys():
        path = reconstructPath(prev, prev[current]) + [current]
        return path
    else:
        return [current]

if __name__ == "__main__":
    #unit testing
    print "Test 1 : Testing getEuclideanDistance"
    assert 1 == getEuclideanDistance((0,0),(1,0))

    print "Test 2 : Testing reconstructPath"
    dir2 = {2:1, 3:2, 4:3, 5:3, 6:5, 7:6, 8:6}
    path2 = reconstructPath(dir2, 8)
    assert path2 == [1,2,3,5,6,8]

