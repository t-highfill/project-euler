
import EulerUtil(mergeUnique, (!!!))

seqS :: [Integer]
seqS = helper 1 2 3 where
  helper a b c = a:helper b c (a+c)

polySets :: Integer -> Integer
polySets n = (helper 0 [1..6] $ drop 3 seqS) !!! (n-4) where
  helper last sums (a:as) = res:helper res newSums as where
    sums' = dropWhile (<=a) sums
    res = last + sum [1 | s<-sums']
    newSums = mergeUnique sums' $ a:[s+a | s<-sums]

