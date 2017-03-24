
import Data.List(genericTake)

repl :: Integral a => a -> b -> [b]
repl n = (genericTake n) . repeat

golomb :: [Integer]
golomb = 1:2:helper [3..] [2] where
  helper (n:ns) (g:gs) = g:helper ns (gs++repl g n)

main = print $ helper 1 [n^3 | n<-[1..10^6-1]] golomb where
  helper n (c:cs) (g:gs) = if n==c then g:helper (n+1) cs gs
                           else helper (n+1) (c:cs) gs
