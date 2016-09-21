

primes :: [Integer]
primes = sieve [2..] where sieve (x:xs) = x:(sieve [y | y<-xs, y `mod` x /= 0])

until_false :: Ord a => [a]->a->(a->a->Bool)->[a]
until_false [] a f = []
until_false (x:xs) a f = if f x a then x:(until_false xs a f) else []

lt_or_eq :: Ord a => [a]->a->[a]
lt_or_eq arr a = until_false arr a (<=)

isprime :: Integer -> Bool
isprime n = n `elem` (lt_or_eq primes n)

intdiv a b = floor $ (fromIntegral a) / (fromIntegral b)

intsqrt n = floor $ sqrt (fromIntegral n)

dis_primefacs :: Integer -> [Integer]
dis_primefacs n = if isprime n then [n] else [x | x<-(lt_or_eq primes n), n `mod` x == 0, n/=x]
--dis_primefacs n = if isprime n then [n] else root:(dis_primefacs $ intdiv n root) where root=head ([p | p<-(lt_or_eq primes n), n `mod` root == 0]++[1])

factorcount = [length (dis_primefacs n) | n<-[0..]]

starts_with arr sub = (take (length sub) arr) == sub

--find_sub [] sub = -1
find_sub arr sub = if arr `starts_with` sub then 0 else 1+(find_sub (tail arr) sub)

makesub n = take n (repeat n)

main = print $ find_sub factorcount (makesub 4)