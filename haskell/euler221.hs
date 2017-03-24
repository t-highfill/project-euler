
import EulerUtil

backFactors n = [i | i<-[start, start-1..1], n `mod` i == 0] where
  start = fst $ intSqrt n

alexInts = mergeInf [subSeq p | p<-[1..]] where
  subSeq p = [p*(p+d)*(p+(pSqr1 `quot` d)) | d<-backFactors $ pSqr1] where
    pSqr1 = (p^2+1)

get (x:_) 0 = x
get (_:xs) n = get xs (n-1)

main = print $ alexInts `get` (150000-1)
