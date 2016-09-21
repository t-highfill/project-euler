#!/usr/bin/python

import Queue, sys

class AddTree(object):
    __slots__ = 'val,depth,parent,_lineage'.split(',')
    def __init__(self, val, parent = None):
        self.val = val
        self.parent = parent
        self.depth = 0 if parent is None else parent.depth +1
        #self.children = []
        self._lineage = None
    def chainVals(self):
        if self._lineage is not None:
            return self._lineage
        res = [self.val]
        if self.parent is not None:
            res += self.parent.chainVals()
        self._lineage = res
        return res
    def tree_str(self, pad = ''):
        t = '\n' if len(self.children) >= 1 else ''
        return '%s%d:val: %d%s%s' % (pad, self.depth, self.val, t,
                '\n'.join(c.tree_str(pad+'\t') for c in self.children))

class minmulGen(object):
    def __init__(self, limit = None, mode = 'pq-'):
        self._knownvals = {1:0}
        self._mode = mode
        self._usePQ = self._mode.startswith('pq')
        self._negate = self._usePQ and self._mode.endswith('-')
        if self._usePQ:
            self._frontier = Queue.PriorityQueue()
        else:
            self._frontier = []
        self.limit = limit
        self.root = AddTree(1)
        self._addNode(self.root)
    def _addNode(self, node):
        if self.limit is None or node.val < self.limit:
            if self._usePQ:
                v = -node.val if self._negate else node.val
                self._frontier.put((node.depth,v,
                                    tuple(node.chainVals()),node))
            else:
                self._frontier.append(node)
    def _getNode(self):
        res = None
        if self._usePQ:
            res =  self._frontier.get()
            res = res[len(res)-1]
        else:
            res = self._frontier.pop(0)
        return res
    def minmul(self, k):
        res = self._knownvals.get(k, None)
        while res is None:
            curr = self._getNode()
            #assert curr.val < self.limit
            vals = curr.chainVals()
            #assert curr.val in vals
            #sys.stdout.write("curr.val=%d  curr.depth=%d  vals=%s\r"
            #                 % (curr.val,curr.depth,vals))
            for v in vals:
                tmp = AddTree(curr.val + v, curr)
                #curr.children.append(tmp)
                self._addNode(tmp)
                if tmp.val not in self._knownvals:
                    self._knownvals[tmp.val] = tmp.depth
                if k == tmp.val:
                    res = tmp.depth
                    #sys.stdout.write("\n")
        return res

def main():
    mm = minmulGen(201, mode='pq')
    #assert mm.limit == 201
    #assert mm.minmul(1) == 0
    #assert mm.minmul(8) == mm.minmul(6) == 3
    #assert mm.minmul(15) == 5
    #print mm.root.tree_str()
    res = 0
    for i in range(1, 201):
        tmp = mm.minmul(i)
        res += tmp
        #print "i=%d\tminmul(i)=%d\tres=%d" % (i, tmp, res)
    print res

if __name__ == '__main__':
    main()
