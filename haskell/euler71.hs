
import Data.Ratio

goal = 3 % 7

candidate d = if n % d < goal then n % d else (n-1) % d where
	  n = (floor $ (fromIntegral d) * 3 / 7)

winner lim = maximum [candidate d | d<-[2..lim]]

main = do
--     print $ winner 8
     print $ winner (10^6)