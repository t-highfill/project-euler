
import Data.Ratio

top :: Double -> Double -> Double
top  n r = 299 - 300*r + (r**n)*(n-299) + (r**(n+1))*(300-n)
top' n r = 300 + n*(r**(n-1))*(n-299) + (n+1)*(r**n)*(300-n)

func  n r = (top n r) + ((1-r)**2)*big where big = 2*(10**11)
func' n r = (oneMr*(top' n r) + 2*(top n r)) / (oneMr**3) where oneMr = 1-r

newton n x0 = x1:newton n x1 where
  x1 = x0 - ((func n x0)/(func' n x0))

main = do
  let n = 5000
  let x0 = 3000
  mapM_ print $ take 50 (newton n x0)
