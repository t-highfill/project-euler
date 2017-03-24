
import EulerUtil

f :: Integer -> Integer
f n = mulMod (n+1) (phi n) $ quot (n^n-1) (n-1)

g :: Integer -> Integer
g n = sum [f i | i<-[2..n]] + 1

main = print $ g (5*10^8)
