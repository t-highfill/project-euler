#!/usr/bin/python

import math, sys

def primes(n):
    print "Generating..."
    prime = {}
    for i in xrange(2,int(math.sqrt(n))):
        if prime.get(i, True):
            prime[i] = True
            for j in xrange(i*i, i, n+1):
                prime[j] = False
    print "Primes generated!"
    for i in xrange(2, n+1):
        sys.stdout.write("Yielding... %f%%\r" % (100*float(i-1)/n))
        if prime.get(i,False):
            yield i

def main():
    ceil = 2**50
    sqFree = {p:True for p in primes(ceil)}
    count = 1
    print "\nCounting..."
    for p in xrange(2,ceil+1):
        if sqFree.get(p, True):
            count +=1
            sys.stdout.write("Count: %d\tFound: %d \r" % (count, p))
            i, j = p*p, 1
            while i*j <= ceil:
                sqFree[i*j] = False
                j += 1
    print "\nDone! Result: %d" % count

if __name__ == '__main__':
    main()
