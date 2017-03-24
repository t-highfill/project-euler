
count = helper 20 [] where
  helper 0 _ = 1
  helper n [] = sum [helper (n-1) [d] | d<-[1..9]]
  helper n [x] = sum [helper (n-1) [x,d] | d<-[0..9-x]]
  helper n [x,y] = sum [helper (n-1) [y,d] | d<-[0..9-x-y]]

main = print count
