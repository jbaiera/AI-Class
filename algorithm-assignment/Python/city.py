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
    members: node, name, xpos, ypos"""

    def __init__(self, number=(-1), name='', x=0, y=0):
        """__init__ :: number(-1) -> name('') -> x(0) -> y(0) -> city"""
        self.node = number
        self.name = name
        self.xpos = x
        self.ypos = y
    #end def

#end class


