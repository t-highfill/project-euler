
import Data.Set(Set)
import Data.Set as Set
import EulerUtil(intSqrt)
import Dijkstra

intSqrt' :: Integral a => a -> a
intSqrt' = fst . intSqrt

inPoints :: Integral a => a -> [(a,a)]
inPoints r = [(x,y) | x<-[0..r], y<-[0..intSqrt' (r^2-x^2)]]

surfPoints :: Integral a => a -> [(a,a,a)]
surfPoints r = [(x,y,z) | (x,y)<-inPoints r, (z,b)<-[intSqrt $ r^2-x^2-y^2], b]

fromIntegPoint (a,b,c) = (fromIntegral a, fromIntegral b, fromIntegral c)

chordDist p1 p2 = helper (fromIntegPoint p1) (fromIntegPoint p2) where
  helper (x1, y1, z1) (x2, y2, z2) = sqrt $ (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2

arcDist :: Integral a => a -> (a,a,a) -> (a,a,a) -> Double
arcDist r p1 p2 = r * 2 * asin (c/2) where c = chordDist p1 p2

danger :: Integral a => a -> (a,a,a) -> (a,a,a) -> Double
danger r p1 p2 = (d/(pi*r))^2 where d = arcDist r p1 p2

buildMoon r = Graph baseSet paths where
  bases = surfPoints r ++ [(x,y,-z) | (x,y,z)<-surfPoints r]
  baseSet = Set.fromList bases
  paths = Set.fromList [Path a b $ danger r a b | a<-bases, b<-bases, a/=b]



