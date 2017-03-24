#!/usr/bin/python

import math

_KNOWN_GCD = {}
def gcd(a, b):
    if b > a:
        return gcd(b, a)
    if (a,b) in _KNOWN_GCD:
        return _KNOWN_GCD[(a,b)]
    res = None
    if b == 0:
        res = a
    else:
        res = gcd(b, a % b)
    _KNOWN_GCD[(a,b)] = res
    return res

class Frac(object):
    def __init__(self, num, den):
        self.num = num
        self.den = den
    def simplify(self):
        d = gcd(self.num,self.den)
        self.num /= d
        self.den /= d
        return self
    def __add__(self, other):
        if not isinstance(other, Frac):
            other = Frac(other, 1)
        return Frac(self.num*other.den + other.num*self.den,
                    self.den*other.den).simplify()

class PrimeGen(object):
    def __init__(self, batchSize=1000):
        self.batchSize = batchSize
        self._ceil = 0
        self._knowns = []
        self._map = {}
        self._curr = 1
        self._idx = 0
        self.grow()
    def _process(self, i):
        if i < math.sqrt(self._ceil):
            for j in xrange(i*i, i, ceil):
                self._map[j] = False
    def grow(self):
        old = self._ceil
        self._ceil += self.batchSize
        for p in self._knowns:
            self._process(p)
    def _growAtNeed(self):
        while self._curr >= self._ceil:
            self.grow()
    def __next__(self):
        if self._idx < len(self._knowns):
            res = self._knowns[self._idx]
            self._idx += 1
            return res
        self._idx += 1
        self._curr += 1
        self._growAtNeed()
        while not self._map.get(self._curr, True):
            self._curr += 1
            self._growAtNeed()
        self._process(self._curr)
        self._knowns.append(self._curr)
        return self._curr
    def __iter__(self):
        self._idx = 0
        return self
    @property
    def knowns(self):
        return self._knowns

_PRIME_GEN = PrimeGen()
    
def primes(gen=_PRIME_GEN):
    for p in gen:
        yield p

def firstDivPrime(n, gen=_PRIME_GEN):
    for p in primes(gen):
        if n % p == 0:
            return p
        
_KNOWN_PHI = {1:1}
def phi(n, gen=_PRIME_GEN):
    if n in _KNOWN_PHI:
        return _KNOWN_PHI[n]
    res = None
    if n % 2 == 0:
        m = n / 2
        if m % 2 == 0:
            res = 2 * phi(m)
        else:
            res = phi(m)
    else:
        p = firstDivPrime(n, gen)
        
    _KNOWN_PHI[n] = res
    return res
