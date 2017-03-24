
import EulerUtil
import Data.List(takeWhile, last)

bigM p q n = helper (n `quot` f) * f where
  f = p*q
  eligible m = (m==p) || (m==q) || (check p) || (check q) where
    check x = m `mod` x == 0 && eligible (m `quot` x)
  helper m = if eligible m then m else helper (m - f)

bigS n = helper primes where
  helper (p:q:ps) = if p*q > n then 0 else helper2 p (q:ps) + helper (q:ps)
  helper2 p (q:ps) = if p*q > n then 0 else bigM p q n + helper2 p ps

main = print $ bigS 100
