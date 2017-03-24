
iPoint r m = (x, r-(sqrt $ x*(2*r-x))) where
  x = ((m*r+r)/(m^2+1)) - (sqrt $ (2*m*r^2)/(m^2+1)^2)

lSecArea r = (r^2)*(4-pi)/4

heron a b c = sqrt $ s*(s-a)*(s-b)*(s-c) where
  s = (a+b+c)/2

dist (x1,y1) (x2,y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2

sliverArea r iP = ((r^2)*theta/2) - bigA where
  d = dist iP (r,0)
  theta = acos $ 1-d/(2*(r^2))
  bigA = heron r r d

smallArea r m = trigA - (sliverArea r (x,y)) where
  (x,y) = iPoint r m
  trigA = r*y/2

percent = (/100)

main = do
  let goal = percent 10
  let r = 1
  let lArea = lSecArea r
  print $ head [n | n<-[3..], (smallArea r (1/n))/lArea < goal]
