#!/usr/bin/python

def squares():
    n = 1
    while True:
        yield n*n
        n+=1

def cubes():
    n = 1
    while True:
        yield n*n*n
        n+=1

def mergeMany(itrs):
    
        
def main():
    count = {}
    result = 0
    found = 0
    for cube in cubes:
        for num in count:
            if num <= cube:
                if count[num] == 4:
                    result += num
                    found += 1
                    print 'found: %d, result: %d' % (num, result)
                    if found == 5:
                        return
                del count[num]
        for sqr in squares():
            if 
