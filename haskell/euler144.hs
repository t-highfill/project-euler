
type Point = (Double, Double)

isOnEllipse :: Point -> Bool
isOnEllipse (x,y) = 4*x^2 + y^2 == 100

canEscape :: Point -> Bool
canEscape (x,y) = isOnEllipse (x,y) && (abs x)<=0.01 && y>0

getYs :: Double -> (Double, Double)
getYs x = (r, -r) where
     r = sqrt $ 100-4*x^2

slopeAt :: Point -> Double
slopeAt (x,y) = (-4)*x/y

perpen :: Double -> Double
perpen m = -(1/m)

slope :: Point -> Point -> Double
slope (x1,y1) (x2,y2) = (y1-y2)/(x1-x2)

dist :: Point -> Point -> Double
dist (x1,y1) (x2,y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2

reflactAngle :: Point -> Point -> Double -> Double
reflectAngle pA pB m = do
	     let dAB = dist pA pB
	     let f x = m*(x-(fst pB))+(snd pB)
	     let mkPoint x = (x, f x)
	     let pD = mkPoint (fst pA)
	     let dBD = dist pB pD
	     let dAD = dist pA pD
	     let alpha = acos $ (dAB^2+dBD^2-dAD^2)/(2*dBD*dAB)
	     let pE = mkPoint $ (fst pB) + (signum $ (fst pB)-(fst pA))

getPoint :: Integer -> (Double, Double)
getPoint 0 = (0.0, 10.1)
getPoint 1 = (1.4, -9.6)
getPoint n = if p1/=plast then p1 else p2 where
	 plast = getPoint (n-1)
	 (x0,y0) = plast
	 
