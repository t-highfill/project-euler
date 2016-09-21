
native_triples :: [(Integer, Integer, Integer)]
native_triples = [(min (sqrdiff m n) (mn2 m n), max (sqrdiff m n) (mn2 m n), m^2 + n^2) 
	       | m<-[1..], n<-[1..m-1], gcd m n == 1, (m-n) `mod` 2 /=0] where 
	       	 sqrdiff m n = m^2 - n^2; mn2 m n = 2*m*n

lessthan :: Ord a => [a] -> a -> [a]
lessthan [] n = []
lessthan (x:xs) n = if x<n then x:(lessthan xs n) else []

intdiv :: Integral a => a -> a -> a
intdiv a b = floor $ (fromIntegral a)/(fromIntegral b)

intersperse :: [a] -> [a] -> [a]
intersperse [] [] = []
intersperse [] other = other
intersperse other [] = other
intersperse (x:xs) (y:ys) = x:y:(intersperse xs ys)

pentags :: [(Integer, Integer)]
pentags = [((m*(3*m-1)) `intdiv` 2, m) | m<-0:(intersperse [1..] [-1,-2..])]

partitions :: Integer -> Integer
partitions 0 = 1
partitions k = if k<0 then 0 else sum [(-1)^m*(partitions (k-pent)) | (pent,m)<-pentags `lessthan` (k,0)]

main = print (partitions 4)