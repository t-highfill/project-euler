
import Data.Ratio

primes :: Integral a => [a]
primes = sieve [2..] where
    sieve [] = error "What the fuck?"
    sieve (p:xs) = p:sieve [x | x<-xs, x `mod` p /= 0]

phi :: Integral a => a -> a
phi 1 = 1
phi n 
    | n `mod` 2 == 0 = if h `mod` 2 == 0 then 2 * phi h else phi h
    | otherwise = numerator $ (n % 1) * (1-(1 % p)) * ((phi n2) % n2) where
    h = quot n 2
    divMax 1 _ = 1
    divMax a 1 = a
    divMax a b = if a `mod` b == 0 then divMax (quot a b) b else a
    n2 = divMax n p
    p = head [p' | p'<-primes, n `mod` p' == 0]

main = do
     let limit = 10^6
     print $ sum [phi d | d<-[2..limit]]