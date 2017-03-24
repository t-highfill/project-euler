#!/usr/bin/python

import sys

class PrimeGen(object):
    def __init__(self, blockSize=1000):
        self.blockSize = blockSize
        self.knowns = set()
        #FIXME
        
def main():
    print list(take(10, primes()))
    groups = {n:{} for n in xrange(0,10)}
    for p in primes():
        p_s = str(p)
        sys.stdout.write("Curr: %s\r" % p_s)
        for i,d in enumerate(reversed(p_s)):
            d = int(d)
            l = len(p_s)
            d_g = groups[d]
            if l not in d_g:
                d_g[l] = {}
            d_g_l = d_g[l]
            res, count = d_g_l.get(i, (p,0))
            count += 1
            d_g_l[i] = (res, count)
            if count >= 8:
                print '\n' + str(res)
                return

if __name__ == '__main__':
    main()
        
