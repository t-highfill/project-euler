
import Data.Ratio

fractran :: Integral a => [Ratio a] -> a -> [a]
fractran prog n = res:fractran prog res where
  res = numerator $ head $ filter (\x->denominator x == 1) [(n%1)*f | f<-prog]

conwayPrimes :: [Integer]
conwayPrimes = fractran [17%91, 78%85, 19%51, 23%38, 29%33, 77%29, 95%23,
                         77%19, 1%17,  11%13, 13%11, 15%2,  1%7,   55%1] 2

isPow2 :: Integral a => a -> Bool
isPow2 1 = True
isPow2 n = rem == 0 && isPow2 d where (d, rem) = divMod n 2

countItrs find = helper conwayPrimes 0 0 where
  helper (p:ps) itrs found | found >= find = itrs
                           | isPow2 p = helper ps (itrs+1) (found+1)
                           | otherwise = helper ps (itrs+1) found

main = print $ countItrs 10001
