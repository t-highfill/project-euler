
import EulerUtil

squares :: [Integer]
squares = [i*i | i<-[1..]]

diffs :: Num a => [a] -> [(a,a,a)]
diffs [] = []
diffs [x] = []
diffs (x:y:xs) = (x,y, y-x):diffs (y:xs)

third :: (a,b,c) -> c
third (_,_,x) = x

search = helper $ diffs squares where
  helper (d1:d2:d3:ds) = if third d1 == third d3 && isEven (third d2)
                         then (d1,d2,d3):cont else cont where
    cont = helper (d2:d3:ds)

triplet = (x,y,z) where
  (d1,d2,_) = head search
  (s2, s3, mid) = d2
  (s1, _, yzDiff) = d1
  z = mid `quot` 2
  x = s2 + z
  y = yzDiff + z

main = print triplet
