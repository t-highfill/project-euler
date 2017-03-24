#!/usr/bin/python

def lagFib():
    k=1
    mod=1000000
    d=[None]
    while True:
        tmp = None
        if k <= 55:
            tmp = (100003 - 200003*k + 300007*k*k*k) % mod
        else:
            tmp = (d[32]+d.pop(1)) % mod
        d.append(tmp)
        yield tmp
        k+=1

