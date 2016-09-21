
import Data.Ratio
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

type Point = (Rational, Rational)
type LineSeg = (Point, Point)

minmax :: Ord a => [a] -> (a, a)
minmax [] = error "Empty List"
minmax [x] = (x,x)
minmax (x:xs) = (min x a, max x b) where (a,b) = minmax xs

numbers :: [Integer]
numbers = drop 1 $ map (`mod` 500) $ helper 290797 where
  helper last = last:(helper $ (last*last) `mod` 50515093)

lineSegs :: [LineSeg]
lineSegs = helper numbers where
  helper (x1:y1:x2:y2:ns) = ((x1 % 1,y1 % 1),(x2 % 1,y2 % 1)):helper ns

slope :: LineSeg -> Rational
slope ((x1,y1),(x2,y2)) = if x1 == x2 then error "Vertical line has no slope"
                          else (y1 - y2)/(x1 - x2)

toPointSlope :: LineSeg -> (Point, Rational)
toPointSlope (p,q) = (p, slope (p,q))

intersection :: LineSeg -> LineSeg -> Point
intersection l1 l2 = helper (toPointSlope l1) (toPointSlope l2) where
  helper ((x1,y1), m1) ((x2,y2), m2) = if m1==m2 then error "Parallel!" else (x,y) where
    x = (y2-m2*x2-y1+m1*x1)/(m1-m2)
    y = m1*(x-x1)+y1

withinBounds :: Point -> LineSeg -> Bool
withinBounds (x,y) ((x1,y1),(x2,y2)) = x >= minX && x <= maxX && y >= minY && y <= maxY where
  (minX, maxX) = minmax [x1, x2]
  (minY, maxY) = minmax [y1, y2]

onLineSeg :: Point -> LineSeg -> Bool
onLineSeg p (p1,p2) = p `elem` [p1,p2]
                      || (withinBounds p (p1,p2)
                          && slope (p,p1) == slope (p,p2))

parallel :: LineSeg -> LineSeg -> Bool
parallel l1 l2 = slope l1 == slope l2

getTIP :: LineSeg -> LineSeg -> Maybe Point
getTIP (a,b) (c,d) = if any id $ [parallel (a,b) (c,d)] ++ [onLineSeg p (c,d) | p<-[a,b]]
                        ++ [onLineSeg p (a,b) | p<-[c,d]] then Nothing else
                       if all (withinBounds result) [(a,b),(c,d)] then Just result else Nothing
  where result = intersection (a,b) (c,d)

countTIPs :: [LineSeg] -> Int
countTIPs lines = helper lines Set.empty where
  helper [] s = Set.size s
  helper (l:ls) s = helper ls $ Set.union s tips where
    tips = Set.fromList $ catMaybes [getTIP l l' | l'<-ls]

main = do
  let space = take 5000 lineSegs
  print $ countTIPs space
