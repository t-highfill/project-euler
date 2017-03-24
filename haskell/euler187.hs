
import EulerUtil

twoFacComps :: [Integer]
twoFacComps = mergeInf [[p*q | q<-(p:ps)] | (p,ps)<-headTails primes]

main = do
  let lim = 10^8
  print $ sum [1 | _<-takeWhile (<lim) twoFacComps]
