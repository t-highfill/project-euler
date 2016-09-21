#!/usr/bin/python

import random, sys

def cutinhalf(n):
    if n == 5:
        return [n]
    return [n+1] + cutinhalf(n+1)

def round6(f):
    return float('%0.6f' % f)

def newenv():
    env = cutinhalf(1)
    env.pop()
    return env

def main():
    batches = last = onesheets = 0
    while True:
        env = newenv()
        while env != [5]:
            batches += 1
            if len(env) == 1:
                onesheets += 1
            i = env.pop(random.randrange(len(env)))
            if i != 5:
                env = sorted(env + cutinhalf(i))
                env.pop()
        f = float(onesheets)/batches
        #if f == last and f != 0:
            #print '\nresult:', f
            #break
        sys.stdout.write('%0.6f  %0.9f\r' % (f,f))
        last = f

if __name__ == '__main__':
    main()
