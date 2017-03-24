
import EulerUtil

gcdN :: Integral a => [a] -> a
gcdN [x] = x
gcdN (1:_) = 1
gcdN (x:y:xs) = gcdN $ (gcd x y):xs

perims :: Integral a => a -> [a]
perims lim = takeWhile (<lim) $ mergeInf [helper c | c<-[1..]] where
  helper c = mergeMany [[a+b+c | a<-[c-b+1..b], gcdN [b,c,a] == 1] | b<-[1..c]]

main = print $ sum [1 | _<-perims (10^7)]
