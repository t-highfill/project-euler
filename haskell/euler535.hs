
import Data.List(genericSplitAt)

intSqrt :: Integral a => a -> a
intSqrt n = floor $ sqrt (fromIntegral n)

fractalSeq :: [Integer]
fractalSeq = [1,1] ++ helper [2..] [1] where
  helper cs (n:bs) = circled ++ n:helper rem (bs++circled++[n]) where
    (circled, rem) = genericSplitAt (intSqrt n) cs

result = helper fractalSeq (10^18) 0 where
  m = 10^9
  helper _ 0 res = res
  helper (f:fs) n res = helper fs (n-1) $ ((f `mod` m) + res) `mod` m

main = print result
