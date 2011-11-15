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

import * from city
