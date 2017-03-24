
import EulerUtil

rects n m = sum [(n-i+1)*(m-j+1) | i<-[1..n], j<-[1..m]]

counts = mergeInf [[(rects n m, n*m) | n<-[1..m]] | m<-[1..]]

search goal = snd $ minimum [(abs $ goal-r, a) | (r,a)<-[p,q]] where
  (p,q) = helper (0,0) counts
  helper last (p:ps) = if fst p > goal then (last,p) else helper p ps

main = print.search $ 2*10^6
