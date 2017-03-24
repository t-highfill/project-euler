
import Data.Numbers.Primes
import Data.List(genericLength)
import Data.Ratio

diags 1 = [1]
diags n = [n^2-i*(n-1) | i<-[3,2..0]]

primeRatio arr = (genericLength $ filter isPrime arr, genericLength arr)

combine (n1, d1) (n2, d2) = (n1+n2, d1+d2)

primeRatios = helper 1 (0, 1) where
  toRat (n,d) = n%d
  helper n r = (n,toRat r):helper (n+2) (combine r $ primeRatio $ diags $ n+2)

main = do
  let goal = 1%10
  print $ head $ dropWhile ((>=goal).snd) $ tail primeRatios
