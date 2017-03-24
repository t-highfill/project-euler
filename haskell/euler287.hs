
data Color = White | Black deriving (Show, Eq)

type CoordT = Int
type Coord = (CoordT, CoordT)

data Region2D = Region2D {botLeft :: Coord, sideLen :: CoordT}
  deriving (Show, Eq)

color :: CoordT -> Coord -> Color
color n (x,y) = if (x-lim)^2 + (y-lim)^2 <= lim^2 then Black else White where
  lim = 2^(n-1)

split :: Region2D -> [Region2D]
split (Region2D (x,y) l) = [Region2D (x+x', y+y') l' | x'<-[0,l'], y'<-[0,l']]
  where l' = quot l 2

bounds :: Region2D -> [Coord]
bounds (Region2D (x,y) l) = [(x+x', y+y') | x'<-[0,l-1], y'<-[0,l-1]]

allEq :: Eq a => [a] -> Bool
allEq (x:xs) = all (==x) xs

codeLen :: CoordT -> Region2D -> Integer
codeLen _ (Region2D _ 1) = 2
codeLen n region = if allEq $ map (color n) $ bounds region then 2
                   else 1 + (sum $ map (codeLen n) $ split region)

bigD n = codeLen n $ Region2D (0,0) (2^n)

main = print $ bigD 24
