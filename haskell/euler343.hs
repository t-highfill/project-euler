
import Data.Ratio
import System.IO(hFlush, stdout)

chain :: Integral a => a -> a
chain k = helper (1%k) where
  helper rat = if d==1 then n else helper $ (n+1)%(d-1) where
    n = numerator rat
    d = denominator rat

partialSums arr = helper arr 0 where
  helper [] s = [s]
  helper (x:xs) s = s:helper xs (x+s)

putOneLine s = do
  putStr $ s ++ "\r"
  hFlush stdout

main = do
  mapM_ (putOneLine.show) $ partialSums [chain (k^3) | k<-[1..2*10^6]]
  putStrLn "\nDone!"
