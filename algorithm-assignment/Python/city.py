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

#module constants
cityNotFound = -1

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
    methods: addCityObject, getNeighbors, addEdgeToStructure, grabNodeTuple, lookupCity
    """
    
    def __init__(self, citylist, edges):
        """__init__ :: [(node, name, x, y)] -> [(from, to, cost)] -> object"""
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
    
    def getNeighbors(self, parentNode=0):
        """getNeighbors :: parentNode(0) -> [(node,cost)]"""
        idx = self.grabNodeTuple(parentNode)
        if idx == -1:
            return [] #no neighbors if there is no node
        else:
            return self.structure[idx][1] #second item (a list) in a node

    def lookupCity(self, nodenumber=0):
        """lookupCity :: nodenumber(0) -> indexofcity"""
        counter = 0
        for eachCity in self.cities:
            if eachCity.node == nodenumber:
                return counter
            else:
                counter = counter + 1
        return -1

    def addCityObject(self, aNewCity):
        """addCityObject :: city -> nothing"""
        self.cities.append(aNewCity)

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
        #now, lets make sure that the node we're travelling to exists.
        if(self.grabNodeTuple(toNode) == (-1)): #if the toNode doesn't exist
            #then add one
            moreStructureData = (toNode, [])
            self.structure.append(moreStructureData)

    def grabNodeTuple(self, targetNode=0):
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
        print 'City Created Successfully.'
    else:
        print 'City Creation Failed. \nTest 1 Failed.'
        exit()

    print 'Test 2: Chart Creation - No Cities.'
    t2_citylist = []
    t2_edges = []
    t2_chart = chart(t2_citylist, t2_edges)
    print 'Chart Created Successfully.'

    print 'Test 3: Add City to chart.'
    t3_chart = t2_chart
    t3_newcity = t1_city
    t3_chart.addCityObject(t3_newcity)
    if(t3_chart.cities[0].name == "London"):
        print 'City Successfully Appended. '
    else:
        print 'City Addition Failed. \nTest 3 Failed.'
        exit()

    print 'Test 4: Grab Node Tuple Index - Empty Structure.'
    t4_chart = t3_chart
    t4_index = t4_chart.grabNodeTuple(3)
    if(t4_index == -1):
        print 'Node Not Found - As Expected. '
    else:
        print 'Incorrect Exit Status. \nTest 4 Failed.'
        exit()
    
    print 'Test 5: Add Edge To Structure.'
    t5_chart = t4_chart
    t5_edgestat = (1,2,5) #from one to two, cost of five
    t5_chart.addEdgeToStructure(*t5_edgestat)
    if (t5_chart.structure == [(1,[(2,5)]),(2,[])]):
        print 'Node and Edge Added Successfully. '
    else:
        print 'Problem occured. Structure : ' , t5_chart.structure
        print 'Test 5 Failed.'
        exit()
    
    print 'Test 6: Grab Existing Node Index.'
    t6_chart = t5_chart
    t6_idx = t6_chart.grabNodeTuple(1)
    if(t6_chart.structure[t6_idx] == (1,[(2,5)])):
       print 'Found Existing Node. '
    else:
        print 'Problem occured. Index : ' , t6_idx
        exit()

    print 'Test 7: Add Another Edge.'
    t7_chart = t6_chart
    t7_edgestat = (1, 3, 7) #from 1 to 3, cost 7
    t7_chart.addEdgeToStructure(*t7_edgestat)
    if(t7_chart.structure == [(1,[(2,5),(3,7)]),(2,[]),(3,[])]):
        print 'Edge Added Successfully. '
    else:
        print 'Problem Occured. Structure : ' , t7_chart.structure
        print 'Test 7 Failed.'
        exit()
    
    print 'Test 8: Create Complex Chart.'
    t8_citylist = [(1,'Downtown',0,0),(2,'Midtown',1,2),(3,'Uptown',4,5)]
    t8_edgelist = [(1,2,4),(2,1,4),(2,3,3),(3,2,3)]
    t8_chart = chart(t8_citylist, t8_edgelist)
    t8_idx = 0
    for eachcity in t8_citylist:
        current = t8_chart.cities[t8_idx]
        if current.name != eachcity[1]:
            print 'Cities Created Incorrectly \nTest 8 Failed.'
            exit()
        else:
            t8_idx = t8_idx + 1
    if(t8_chart.structure == [(1,[(2,4)]),(2,[(1,4),(3,3)]),(3,[(2,3)])]):
        print 'Structure and Cities created successfully. '
    else:
        print 'Structure Incorrect : ' , t8_chart.structure
        print 'Test 8 Failed.'
        exit()

    print 'Test 9: Pulling Neighbors.'
    t9_chart = t8_chart
    t9_neighbors = t9_chart.getNeighbors(2)
    if(t9_neighbors == [(1,4),(3,3)]):
        print 'Neighbors found. '
    else:
        print 'Incorrect Neighbors : ' , t9_neighbors
        print 'Test 9 Failed.'
        exit()
    
    print 'Test 10: City Lookup.'
    t10_chart = t9_chart
    t10_city = t8_citylist[0]
    t10_idx = t10_chart.lookupCity(1)
    if(t10_chart.cities[t10_idx].node == 1):
        print 'City Found.'
    else:
        print 'City not found.' , t10_idx
        print 'Test 10 Failed!'
        exit()

    print 'Testing Finished.'


