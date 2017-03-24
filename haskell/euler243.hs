
import EulerUtil
import Data.Ratio

resillience :: Integer -> Rational
resillience d = (phiRatio d)/((d%1)-1)

main = do
  let goal = 15499 % 94744
  --let goal = 4%10
  print $ head [d | (d,phiD)<-tail totients, phiD%(d-1) < goal]
