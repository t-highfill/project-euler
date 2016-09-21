
import Data.List (takeWhile)

addMod :: Integral a => a -> a -> a -> a
addMod m a b = ((a `mod` m) + (b `mod` m)) `mod` m

sumMod :: Integral a => a -> [a] -> a
sumMod m xs = foldr (addMod m) 0 xs

digSum :: Integral a => a -> a
digSum n = if n < 10 then n else d+(digSum n') where
  (n', d) = divMod n 10

kVals :: [Integer]
kVals = [k | k<-[23,46..], digSum k == 23]

bigS m n = sumMod m $ takeWhile (<limit) kVals where limit = 10^n

main = print $ bigS (10^9) (11^12)
