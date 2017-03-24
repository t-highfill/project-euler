#!/usr/bin/python

import math

def primes(limit):
    comps = set()
    root = int(math.sqrt(limit))
    for i in xrange(2,limit):
        if i in comps:
            comps.remove(i)
        else:
            yield i
            if i < root:
                comps = comps.union(xrange(i*i, limit, i))

def keys(n):
    s = str(n)
    return [s[:i]+'*'+s[i+1:] for i in xrange(len(s))] + ['*'+s, s+'*']
        
def bigF(limit):
    rel2 = {2}
    conns = {}
    res = 0
    def isRel2(p, omit=set(), rel2=rel2, conns=conns):
        if p in rel2:
            return True
        matches = set()
        for k in keys(p):
            matches |= conns.get(k,set())
        return any(isRel2(m, omit | {p}) for m in matches - omit)
    for p in primes(limit):
        tmp = isRel2(p)
        for k in keys(p):
            m = conns.get(k,set())
            m.add(p)
            conns[k] = m
        if not tmp:
            res += p
        else:
            rel2.add(p)
    return res
            
def main():
    print bigF(10000)

if __name__ == '__main__':
    main()
