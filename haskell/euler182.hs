
import RSA
import EulerUtil

unconcealed :: Num a => Key -> a
unconcealed key = sum [1 | m<-[0..(modVal key)-1], m == runKey key m]

main = do
  let (p,q) = (1009, 3643)
      n = p*q
      phiN = (p-1)*(q-1)
      keys = [Key e n | e<-[2..phiN-1], areCoprime e (phi phiN)]
      minSum = helper keys (n+1,0) where
        helper [] res = res
        helper (k:ks) (uncon, esum) = helper ks next where
          u = unconcealed k
          e = keyVal k
          next = if uncon == u then (u, esum+e)
                 else if u < uncon then (u, e) else (uncon, esum)
  print minSum
