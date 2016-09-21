
module EulerUtil (
    addMod,
    assert,
    coeffMul,
    coeffRec,
    distinctPrimeFactors,
    expMod,
    intSqrt,
    isEven,
    isInt,
    isOdd,
    isPrime,
    mulMod,
    phi,
    phiRatio,
    primeFactors,
    primes,
    productMod,
    solveQuadratic,
    subMod,
    sumMod,
    withMod,
    ) where

import Data.Ratio

isEven :: Integral a => a -> Bool
isEven a = a `mod` 2 == 0

isOdd :: Integral a => a -> Bool
isOdd a = not $ isEven a

isInt :: RealFrac a => a -> Bool
isInt x = (floor x :: Integer) == (ceiling x :: Integer)

intSqrt :: Integral a => a -> (a, Bool)
intSqrt x = (floor root, isInt root) where root = sqrt (fromIntegral x :: Double)

solveQuadratic :: Floating a => a -> a -> a -> (a,a)
solveQuadratic a b c = (use (+), use (-)) where
    use f = f (-b) (sqrt $ b*b - 4*a*c) / (2*a)

primes :: Integral a => [a]
primes = sieve [2..] where
    sieve [] = error "Infinite list ended somehow"
    sieve (p:xs) = p:sieve [x | x<-xs, x `mod` p /= 0]

isPrime :: Integral a => a -> Bool
isPrime 1 = False
isPrime n = helper $ takeWhile (<=rt) primes where
  rt = ceiling $ sqrt (fromIntegral n)
  helper [] = True
  helper (x:xs) = n `mod` x /= 0 && helper xs

firstDivPrime :: Integral a => a -> a
firstDivPrime n = head [p | p<-primes, n `mod` p == 0]

divMax :: Integral a => a -> a -> a
divMax 1 _ = 1
divMax a 1 = a
divMax a b = if a `mod` b == 0 then divMax (quot a b) b else a

distinctPrimeFactors :: Integral a => a -> [a]
distinctPrimeFactors 1 = []
distinctPrimeFactors n = p:distinctPrimeFactors (divMax n p) where
    p = firstDivPrime n

primeFactors :: Integral a => a -> [a]
primeFactors 1 = []
primeFactors n = p:primeFactors (quot n p) where
    p = firstDivPrime n

phiRatio :: Integral a => a -> Ratio a
phiRatio 1 = 1
phiRatio n
    | isEven n = if isEven halfn then 2*halfphi else halfphi
    | otherwise = (n % 1) * (1-(1 % p)) * (phiRatio n2 / (n2 % 1)) where
        halfn = quot n 2
        halfphi = phiRatio halfn
        n2 = divMax n p
        p = firstDivPrime n

phi :: Integral a => a -> a
phi n = numerator $ phiRatio n

coeffRec :: Integral a => a -> a -> a
coeffRec _ 0 = 1
coeffRec n k = if n==k then 1 else coeffRec (n-1) (k-1) + coeffRec (n-1) k

coeffMul :: Integral a => a -> a -> a
coeffMul n k = numerator $ product [(n+1-i) % i | i<-[1..k]]

withMod :: Integral a => (a -> a -> a) -> a -> a -> a -> a
withMod f m a b = (f (a `mod` m) (b `mod` m)) `mod` m

addMod :: Integral a => a -> a -> a -> a
addMod = withMod (+)

mulMod :: Integral a => a -> a -> a -> a
mulMod = withMod (*)

expMod :: Integral a => a -> a -> a -> a
expMod 1 _ _ = 0
--expMod m a 2 = mulMod m a a
--expMod m a 1 = a `mod` m
expMod m _ 0 = 1 `mod` m
expMod m a b = if b < 0 then error "Negative exponent" else
                 if isEven b then
                  mulMod m halfb halfb else
                  mulMod m a (expMod m a $ b-1)
    where halfb = expMod m a (quot b 2)

subMod :: Integral a => a -> a -> a -> a
subMod = withMod (-)

sumMod :: Integral a => a -> [a] -> a
sumMod m = foldr (addMod m) 0

productMod :: Integral a => a -> [a] -> a
productMod m = foldr (mulMod m) 1

assert :: Bool -> IO ()
assert test = if test then putStr "" else error "Assertion failed!"
