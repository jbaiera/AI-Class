##############
# searchtest #
################################################################################
# James Baiera                                                                 #
# Kent State University                                                        #
# 11-16-2011                                                                   #
#                                                                              #
# This file contains a map of the USA in graph format. We will use this data   #
#   to test our A* search algorithm                                            #
################################################################################

import astar
import city

citylist = [(1, "Augusta", 35,24),(2, "Albany", 31,22),(3, "Hartford", 34, 21),(4, "Cleveland", 27,20),(5, "Dover", 33,17),(6, "Columbia", 35, 12),(7, "Lansing", 24, 21),(8, "Flint", 24, 23),(9, "Chicago", 21, 20),(10, "Atlanta", 29, 9),(11, "Tallahassee", 32, 6),(12, "Miami", 34, 3),(13, "New Orleans", 25, 6),(14, "St. Paul", 19, 22),(15, "Apostle", 22, 24),(16, "Topeka", 19, 14),(17, "Fargo", 18, 25),(18, "El Paso", 16, 6),(19, "Tucson", 11, 5),(20, "Helena", 10, 24),(21, "Boise", 9, 20),(22, "Reno", 6, 16),(23, "Los Angeles", 4, 8),(24, "San Francisco", 2, 14),(25, "Portland", 3, 21),(26, "Seattle", 3, 25),(27, "Austin", 20, 2),(28, "Santa Fe", 13, 11)]
edgelist = [(1,2,5),(1,3,3),(2,1,5),(2,3,3),(2,4,5),(3,1,3),(3,2,3),(3,5,6),(4,2,5),(4,5,7),(4,7,3),(4,10,15),(5,3,6),(5,4,7),(5,6,9),(6,5,9),(6,10,8),(6,11,10),(7,4,3),(7,8,3),(7,9,4),(8,7,3),(9,7,4),(9,14,4),(9,16,8),(10,4,15),(10,6,8),(10,11,7),(10,13,6),(10,16,15),(11,6,10),(11,10,7),(11,13,8),(11,12,6),(12,11,6),(13,10,6),(13,11,8),(13,27,9),(14,9,4),(14,15,5),(14,17,4),(15,14,5),(15,17,5),(16,9,8),(16,10,15),(16,22,17),(16,28,8),(17,14,4),(17,15,5),(17,20,9),(18,19,7),(18,27,10),(18,28,8),(19,18,7),(19,23,11),(20,17,9),(20,21,5),(20,26,8),(21,20,5),(21,22,7),(22,16,17),(22,21,7),(22,23,11),(22,25,9),(22,28,12),(23,19,11),(23,22,11),(23,24,9),(24,23,9),(24,25,9),(25,22,9),(25,24,9),(25,26,5),(26,20,8),(26,25,5),(27,13,9),(27,18,10),(28,16,8),(28,18,8),(28,22,12)]

mychart = city.chart(citylist,edgelist)

print "1 to 7: ",
testpath = astar.Astar(1,7,mychart)
print testpath

print "17 to 1: ",
testpath = astar.Astar(17, 1, mychart)
print testpath

print "1 to 24: ",
testpath = astar.Astar(1, 24, mychart)
print testpath

print "1 to 26: ",
testpath = astar.Astar(1, 26, mychart)
print testpath

print "1 to 17: ",
testpath = astar.Astar(1, 17, mychart)
print testpath

print "1 to 19: ",
testpath = astar.Astar(1, 19, mychart)
print testpath

print "1 to 27: ",
testpath = astar.Astar(1, 27, mychart)
print testpath

print "0 (non-existant) to 1: ",
testpath = astar.Astar(0, 1, mychart)
print testpath


