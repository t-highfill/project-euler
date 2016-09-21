primes :: [Integer]
primes = sieve [2..] where sieve (x:xs) = x:(sieve [y | y<-xs, y `mod` x /= 0])

less_than :: Ord a => [a] -> a -> [a]
less_than [] n = []
less_than (x:xs) n = if x<n then x:(less_than xs n) else []

plt :: Integer -> [Integer]
plt n = primes `less_than` n

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = (quicksort [y | y<-xs, y<=x]) ++ x:(quicksort [y | y<-xs, y>x])

sort_all :: Ord a => [[a]] -> [[a]]
sort_all arr = map quicksort arr

uniques :: Eq a => [a] -> [a]
uniques [] = []
uniques (x:xs) = x:(uniques [y | y<-xs, y/=x])

--prime_sums :: Integer -> Integer
--prime_sums 4 = 1
prime_sums 2 = [[2]]
prime_sums 3 = [[3]]
prime_sums n = if n<2 then [[]] else 
	   uniques $ sort_all $ [arr | arr<-(concat [if a+b==n then [[a,b]] else [[a,b]++xs | xs<-prime_sums (n-a-b)] | a<-(plt n), b<-a:(plt a)]), sum arr == n]

main = do
     let maketup n = (length (prime_sums n),n)
     let failures=[maketup n | n<-[4..]] `less_than` (5000,0)
     let disp a = putStr $ (show a)++"\r"
     map disp failures
     let (len, n) = maximum failures
     disp $ maketup (n+1)
     return []
     
     