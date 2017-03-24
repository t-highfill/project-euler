
import Data.Ratio
import Data.List(delete)

data Slope a = Vertical | Slope (Ratio a) deriving (Show, Eq)

--type MyInt = Int

--points :: MyInt -> [(MyInt, MyInt)]
points lim = tail [(x,y) | x<-[0..lim], y<-[0..lim]]


trigs lim = [((0,0), p, q) | p<-points lim, q<-filter (<p) $ points lim]

slope (x1, y1) (x2, y2) = if x1 == x2 then Vertical else Slope $ (y1-y2)%(x1-x2)

perpen Vertical = Slope 0
perpen (Slope 0) = Vertical
perpen (Slope r) = Slope $ -1/r

isRight (o, p, q) = any id [slope a b == perpen (slope a c)
                           | a<-pts, let [b,c]=delete a pts] where pts=[o,p,q]

--rightTrigs :: MyInt -> [(MyInt, MyInt, MyInt)]
rightTrigs = (filter isRight).trigs

main = print $ length $ rightTrigs 50
