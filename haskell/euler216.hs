
import EulerUtil

lilT :: Integer -> Integer
lilT n = 2 * n^2 - 1

vals lim = [lilT n | n<-[2..lim]]

result lim = sum [1 | x<-intersection primes (vals lim)]

main = print $ result 10000
