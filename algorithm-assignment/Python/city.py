########
# city #
################################################################################
# James Baiera                                                                 #
# Kent State University                                                        #
# 11-8-2011                                                                    #
#                                                                              #
# This file contains the object for cities and charts of cities. It is         #
#   essentially a weighted graph library for python to be used in the A*       #
#   algorithm homework.                                                        #
# The object needs to take a list of edges as input, as well as a list of      #
#   nodes and information about them. It is based off of that information that #
#   we will build a heuristic to traverse the graph.                           #
################################################################################

class city:
    """city:
    creation: Mycity = city(node, name, xpos, ypos)
    purpose: container for city information. Used for lookups.
    members: node, name, xpos, ypos
    """

    def __init__(self, number=(-1), name='', x=0, y=0):
        """__init__ :: number(-1) -> name('') -> x(0) -> y(0) -> city"""
        self.node = number
        self.name = name
        self.xpos = x
        self.ypos = y

#end class

class chart:
    """chart:
    creation: Mychart = chart([(node, name, x, y)],[(from,to,cost)])
    purpose: holds structure of graph, and lookup table for node data.
    members: cities, structure
    methods: addCityObject, getNeighbor, addEdgeToStructure, grabNodeTuple
    """
    
    def __init__(self, citylist, edges):
        """___init___ :: [(node, name, x, y)] -> [(from, to, cost)] -> object"""
        #Initialize, parse the lookup, then the structure
        self.structure = []
        self.cities = []

        #Parse the cities into a lookup.
        if (len(citylist) != 0):
            for eachTuple in citylist:
                #YEAH TUPLE UNPACKING! (I finally used it...)
                newCity = city(*eachTuple)
                self.addCityObject(newCity)

        #Parse the edges into a graph structure.
        if (len(edges) != 0):
            for eachEdge in edges:
                #lets do it again!
                self.addEdgeToStructure(*eachEdge)
    
    def addCityObject(self, aNewCity):
        """addCityObject :: city -> nothing"""
        self.cities = append(aNewCity)

    def addEdgeToStructure(self, fromNode, toNode, cost):
        """addEdgeToStructure :: from -> to -> cost -> nothing"""
        #search for the index for the node that we start at.
        structIdx = self.grabNodeTuple(fromNode)
        if(structIdx != (-1)): #if we found it,
            newFlight = (toNode, cost)
            self.structure[structIdx][1].append(newFlight)
        else: #if the from node doesn't exist
            newStructureData = (fromNode, [(toNode, cost)])
            self.structure.append(newStructureData)
    
    def grabNodeTuple(self, targetNode=0)
        """grabNodeTuple :: target node(0) -> index in structure(-1 on error)"""
        targetIdx = 0
        for eachTuple in self.structure:
            if eachTuple[0] == targetNode:
                #found what we're looking for
                return targetIdx
            else:
                #increment and keep searching
                targetIdx = targetIdx + 1
        #at this point we will have not found it.
        return -1;

#end class

if __name__ == '__main__':
    #TESTING TIME:

    print 'Test 1: City Creation.'
    t1_stats = (1, "London", 6, 54)
    t1_city = city(*t1_stats)
    if((t1_city.node == 1) and (t1_city.name == "London") 
        and (t1_city.xpos == 6) and (t1_city.ypos == 54)):
        print 'City Created Successfully. \nTest 1 Complete.'
    else:
        print 'City Creation Failed. \nTest 1 Failed.'
        exit()

    print 'Test 2: Chart Creation - No Cities.'
    t2_citylist = []
    t2_edges = []
    t2_chart = chart(t2_citylist, t2_edges)
    print 'Chart Created Successfully. \nTest 2 Complete.'

    print 'Test 3: Add City to chart.'
    t3_chart = t2_chart
    t3_newcity = t1_city
    t3_chart.addCityObject(t3_newcity)
    if(t3_chart.cities[0].name == "London"):
        print 'City Successfully Appended. \nTest 3 Complete.'
    else:
        print 'City Addition Failed. \nTest 3 Failed.'
        exit()

    #test grab Node Tuple for not finding something
    #test add edge to structure
    #test grab node again on the new node
    #add another edge to the new node
    #build a basic chart from init
    #test on complex chart


