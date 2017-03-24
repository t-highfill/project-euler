#!/usr/bin/python

def triangle(n):
    return n*(n+1)/2

def square(n):
    return n*n

def pentagonal(n):
    return n*(3*n-1)/2

def hexagonal(n):
    return n*(2*n-1)

def heptagonal(n):
    return n*(5*n-3)/2

def octagonal(n):
    return n*(3*n-2)

def polys(n):
    return [triangle(n), square(n), pentagonal(n),
            hexagonal(n), heptagonal(n), octagonal(n)]

def main():
    graph = {}
    membership = {}
    n = 1
    while triangle(n) <= 9999:
        for i,p in enumerate(polys(n)):
            if p >= 1000:
                membership[p] = i+3
                pstr = str(p)
                prefix = pstr[:2]
                graph[prefix] = graph.get(prefix, []) + [p]
                cycle = [p]
                
        n += 1
