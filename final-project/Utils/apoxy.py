#!/usr/bin/python

import sys

def main():
    """main :: None
    Called on program run"""

    resultFile = sys.argv[1]
    with open(resultFile, 'r') as input:
        for eachRun in range(0,16):
            for eachStep in range(0,7):
                exec(input.readline())
            print p1 , " -- " , p2
            print (p1s / 10.0) , "% -- " , (p2s / 10.0) , "%"
    input.close()
#end def

if __name__ == "__main__":
    main()

