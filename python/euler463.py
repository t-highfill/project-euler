#!/usr/bin/python

import sys

KNOWN = {1:1, 3:3}
def funky(n):
    global KNOWN
    res = None
    if n % 2 == 0:
        res = KNOWN[n] = KNOWN[n/2]
    elif (n-1) % 4 == 0:
        r = (n-1) / 4
        res = KNOWN[n] = 2*KNOWN[2*r+1] - KNOWN[r]
    else:
        r = (n-3) / 4
        res = KNOWN[n] = 3*KNOWN[2*r+1] - 2*KNOWN[r]
        del KNOWN[r]
    return res

def funkySum(n):
    print "n=%d" % n
    mod = 10**9
    res = (1+3+funky(2)) % mod
    for i in xrange(4,n+1):
        res += funky(i) % mod
        res = res % mod
        sys.stdout.write("\ri=%d" % i)
    print "Done!"
    return res

def main():
    print funkySum(3**37)

if __name__ == '__main__':
    main()
