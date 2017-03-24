
import EulerUtil

digits :: Integral a => a-> [a]
digits n = if n < 10 then [n] else (digits q) ++ [r] where
    (q,r) = divMod n 10

segs :: Integral a => a -> [Bool]
segs n = map (=='1') $ helper n where
    helper 0 = "1" ++ "11" ++ "0" ++ "11" ++ "1"
    helper 1 = "0" ++ "01" ++ "0" ++ "01" ++ "0"
    helper 2 = "1" ++ "01" ++ "1" ++ "10" ++ "1"
    helper 3 = "1" ++ "01" ++ "1" ++ "01" ++ "1"
    helper 4 = "0" ++ "11" ++ "1" ++ "01" ++ "0"
    helper 5 = "1" ++ "10" ++ "1" ++ "01" ++ "1"
    helper 6 = "1" ++ "10" ++ "1" ++ "11" ++ "1"
    helper 7 = "1" ++ "11" ++ "0" ++ "01" ++ "0"
    helper 8 = "1" ++ "11" ++ "1" ++ "11" ++ "1"
    helper 9 = "1" ++ "11" ++ "1" ++ "01" ++ "1"

digTrans :: Integral a => a -> a -> a
digTrans n m = sum [1 | b<-zipWith (/=) (segs n) (segs m), b]

transitions :: Integral a => a -> a -> a
transitions n m = if n == m then 0 else (digTrans nd md) + (transitions nq mq) where
    (nq, nd) = divMod n 10
    (mq, md) = divMod m 10

segCount :: Integral a => a -> a
segCount n = sum [sum [1 | b<-segs d, b] | d<-digits n]

samTrans :: Integral a => [a] -> a
samTrans [] = 0
samTrans (n:ns) = (segCount n)*2 + (samTrans ns)

maxTrans :: Integral a => [a] -> a
maxTrans [] = 0
maxTrans (x:xs) = segCount x + helper (x:xs) where
    helper [n] = segCount n
    helper (n:m:ns) = transitions n m + helper (m:ns)

numbers = takeWhile (<(2*big)) $ dropWhile (<big) primes where
    big = 10^7

digitalRoots :: Integral a => a -> [a]
digitalRoots n = if n < 10 then [n] else dsum:digitalRoots dsum where
    dsum = sum $ digits n

main = print $ sum [diff p | p<-primes] where
    diff p = (samTrans roots) - (maxTrans roots) where
        roots = digitalRoots p