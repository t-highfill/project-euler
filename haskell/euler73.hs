
import Data.Ratio
import Data.List (takeWhile)

candidates d = sum [1 | n<-[quot d 3..quot d 2], (n%d) > (1%3), (n%d) < (1%2), gcd n d == 1]

answer lim = sum [candidates d | d<-[2..lim]]

main = print $ answer (10^6)