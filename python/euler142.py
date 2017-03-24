#!/usr/bin/python

def squares(limit=None):
    i=1
    while (limit is None) or i*i < limit:
        yield i*i
        i += 1
        
def main():
    midpoints = {}
    for sq1 in squares():
        for sq2 in squares(sq1):
            diff = sq1-sq2
            if diff % 2 == 0:
                mid = (sq1+sq2)/2
                ranges = midpoints.get(mid, [])
                ranges.append((diff,sq2,sq1))
                midpoints[mid] = ranges
                if len(ranges) >= 2:
                    

if __name__ == '__main__':
    main()
