
import Data.List(takeWhile)

primes :: [Integer]
primes = sieve [2..] where
  sieve (p:ps) = p:sieve [q | q<-ps, q `mod` p /= 0]

isInt f = floor f == ceiling f

intSqrt :: Integral a => a -> (a, Bool)
intSqrt n = (floor f, isInt f) where
  f = sqrt $ fromIntegral n

weirdNs :: [Integer]
weirdNs = helper primes where
  helper ps = if noSolution then helper (tail ps) else n:(helper (tail ps)) where
    (nsqr:others) = zipWith (-) ps [1,3,7,9,13,27]
    (n, perfect) = intSqrt nsqr
    noSolution = any (/=nsqr) others || (not perfect)

main = do
  let limit = 10^6
  print $ sum $ takeWhile (<limit) weirdNs
