#!/usr/bin/python

import sys

def main():
    d = {}
    leader = (0,None)
    k = 3
    while leader[0] != 5:
        cube = k**3
        key = ''.join(sorted(str(cube)))
        hits, c = d.get(key, (0, cube))
        hits += 1
        d[key] = tup = (hits, c)
        sys.stdout.write('k=%d  cube=%d  tup=%s\r'
                         % (k, cube, str(tup)))
        if hits > leader[0]:
            leader = tup
            print '\nleader is now', leader
        k += 1
    print 'result =', leader[1]

if __name__ == '__main__':
    main()
