
import Data.List

splits arr = [splitAt i arr | i<-[1..(length arr)-1]]

numbers = nub.(concatMap helper).permutations where
  check a b = if b/=0 && a `mod` b == 0 then [quot] else []
  helper [a] = [a]
  helper arr' = nub [f a b | (l,r)<-splits arr', a<-helper l, b<-helper r,
                     f<-[(+),(-),(*)]++check a b]

posNums = (filter (>0)).numbers

lastNat arr = head $ [n-1 | (n,g)<-zip [1..] nums, n/=g] ++ [maximum nums]
  where nums = sort $ posNums arr

digLists = [[a,b,c,d] | d<-[0..9], c<-[0..d-1], b<-[0..c-1], a<-[0..b-1]]

main = print $ maximum [(lastNat arr, arr) | arr<-digLists]
