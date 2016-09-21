#!/usr/bin/python

def subsets(bigset):
    setlist = list(bigset)
    for size in range(1,len(bigset)-1):
        for i in range(len(bigset)-size):
            yield set(setlist[i:i+size])

def sign(n):
    if n==0:
        return 0
    return n/abs(n)

def isSpecial(A, B, C):
    assert  A.issuperset(B) and A.issuperset(C)
    assert B.isdisjoint(C)
    sumB , sumC = sum(B), sum(C)
    lendiff = sign(len(B)-len(C))
    return sumB != sumC and (lendiff==sign(sumB-sumC) or lendiff==0)

def main():
    sets = None
    with open('p105_sets.txt','r') as f:
        sets = [{int(s) for s in line.split(',')} for line in f.read().splitlines()]
    res = 0
    for A in sets:
        special = True
        for B in subsets(A):
            C = A-B
            special = special and all(isSpecial(A, B, c) for c in [C]+list(subsets(C)))
            if not special:
                break
        if special:
            res+=sum(A)
            print A
    print res

if __name__ == '__main__':
    main()
