
import EulerUtil
import Data.List (takeWhile)

phiChain :: Integral a => a -> a
phiChain 1 = 1
phiChain n = 1+(phiChain $ phi n)

main = do
  let limit = 40000000
  let len = 25
  let ps = takeWhile (<limit) primes
  let pChain p = 1+(phiChain $ p-1)
  print $ sum [1 | p<-ps, pChain p == len]
