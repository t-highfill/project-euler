
import EulerUtil

hammingNums :: Integer -> Integer -> Integer
hammingNums lim order = helper 1 where
  relevant = takeWhile (<=order) primes
  helper n = if n > lim then 0 else 1 + sum [helper (n*p) | p<-relevant]

main = print $ hammingNums (10^8) 5
