
import Data.Numbers.Primes

truncL n = if n < 10 then [n] else n:truncL (read $ tail $ show n)
truncR n = if n < 10 then [n] else n:truncR (quot n 10)

isTruncatable n = all isPrime $ (tail $ truncL n) ++ (tail $ truncR n)

main = print $ sum $ take 11 $ filter isTruncatable $ dropWhile (<10) primes
