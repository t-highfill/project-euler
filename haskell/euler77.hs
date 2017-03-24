
import Data.Numbers.Primes
import EulerUtil(mergeInf, headTails, streaks)

primeSums = helper 0 primes where
  helper n ps = n:mergeInf [helper (n+p) (p:ps') | (p,ps')<-headTails ps]

main = print $ head [n | (n,s)<-streaks primeSums, s>5000]
