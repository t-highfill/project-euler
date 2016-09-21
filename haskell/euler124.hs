primes = sieve [2..] where sieve (x:xs) = x:(sieve [y | y<-xs, y `mod` x/=0])

isprime n = search primes n where
	search (x:xs) n = if x==n then True else if x>n then False else search xs n

less_than [] lim = []
less_than (x:xs) lim = if x>=lim then [] else if xs==[] then [x] else x:(less_than xs lim)

dis_primefacs n = if isprime n then [n] else [x | x<-(primes `less_than` (floor $ sqrt $ fromIntegral n))]

prod [] = 1
prod (x:xs) = x*(prod xs)

rad n = prod $ dis_primefacs n

quicksort [] = []
quicksort (x:[]) = [x]
quicksort (x:xs) = (quicksort [y | y<-xs, y<=x]) ++ x:(quicksort [y | y<-xs, y>x])

all_rads lim = quicksort [(rad n, n) | n<-[1..lim]]

res k lim = snd $ (all_rads lim) !! (k-1)

main = do
     print $ res 4 10
     print $ res 6 10
     print $ res 10000 100000