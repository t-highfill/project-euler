
module EulerUtil where

import System.IO
import Data.Numbers.Primes
import Data.Ratio

data PythTriple a = PythTriple {triple :: (a,a,a),
                                tripleCmp :: (PythTriple a ->
                                              PythTriple a -> Ordering)}
            
instance Eq (PythTriple a) where
  (==) t1 t2 = (tripleCmp t1) t1 t2 == EQ

instance Ord (PythTriple a) where
  compare t1 t2 = (tripleCmp t1) t1 t2

instance Show a => Show (PythTriple a) where
  show t = "Triple " ++ show (triple t)

data PrimeFacs a = PrimeFacs {pfactors :: [a],
                             original :: a} deriving (Show, Eq, Ord)

perim :: Num a => PythTriple a -> a
perim (PythTriple (a,b,c) _) = a+b+c

facsToNum :: Num a => PrimeFacs a -> a
facsToNum = product . pfactors

mulFacs :: (Num a, Ord a) => PrimeFacs a -> PrimeFacs a -> PrimeFacs a
mulFacs a b = PrimeFacs (merge (pfactors a) (pfactors b))
              $ (original a) * (original b)

streaks :: (Eq a, Num b) => [a] -> [(a, b)]
streaks [] = []
streaks (x:xs) = streak x xs 1 where
  streak y' [] s = [(y',s)]
  streak y' (y:ys) s = if y' == y then streak y' ys (s+1)
                       else (y', s):streaks (y:ys)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort [a] = [a]
qsort (x:xs) = (qsort [y | y<-xs, y<=x]) ++ x:qsort [y | y<-xs, y>x] 

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x < y then x:merge xs (y:ys) else y:merge (x:xs) ys

mergeUnique :: Ord a => [a] -> [a] -> [a]
mergeUnique [] [] = []
mergeUnique [] ys = ys
mergeUnique xs [] = xs
mergeUnique (x:xs) (y:ys)
  | x==y = x:mergeUnique (clean x xs) (clean x ys)
  | x<y       = x:mergeUnique (clean x xs) (y:ys)
  | otherwise = y:mergeUnique (x:xs) (clean y ys) where
      clean n = dropWhile (==n)

mergeMany :: Ord a => [[a]] -> [a]
mergeMany [] = []
mergeMany [a] = a
mergeMany ([]:as) = mergeMany as
mergeMany arrs = mergeMany $ helper arrs where
  helper [] = []
  helper [a] = [a]
  helper (a:b:as) = (merge a b):helper as

mergeSort :: Ord a => [a] -> [a]
mergeSort arr = mergeMany [[a] | a<-arr]

mergeInf :: Ord a => [[a]] -> [a]
mergeInf arrs = helper [] arrs where
  helper seqs [] = mergeMany seqs
  helper seqs [a] = mergeMany (a:seqs)
  helper seqs ([]:as) = helper seqs as
  helper ([]:seqs) as = helper seqs as
  helper seqs (a:as) = mergeMany fronts ++ helper (a:backs) as where
    spans = map (span (<(head a))) seqs
    fronts = map fst spans
    backs = map snd spans

headTails :: [a] -> [(a,[a])]
headTails [] = []
headTails (x:xs) = (x,xs):headTails xs

primeFacs :: Integral a => [PrimeFacs a]
primeFacs = map (\(n,fs)->PrimeFacs fs n) $ helper [] 1 primes where
  helper arr n ps = (n,arr):mergeInf
    [helper (arr++[p]) (n*p) (p:ps') | (p,ps')<-headTails ps]

totients :: Integral a => [(a,a)]
totients = helper primeFacs where
  distinct [] = []
  distinct [p] = [p]
  distinct (x:y:xs) = if x==y then distinct (y:xs) else x:distinct (y:xs)
  phiFac n fs = numerator $ (n%1) * product [1-(1%p) | p<-fs]
  helper ((PrimeFacs fs n):pfs) = (n, phiFac n $ distinct fs):helper pfs

intersection :: Ord a => [a] -> [a] -> [a]
intersection _ [] = []
intersection [] _ = []
intersection (x:xs) (y:ys) | x==y = x:intersection xs ys
                           | x> y = intersection (x:xs) ys
                           | x< y = intersection xs (y:ys)

pythTriplesCmp :: Integral a => (PythTriple a -> PythTriple a -> Ordering) -> [PythTriple a]
pythTriplesCmp cmp = mergeInf [qsort [trip m n | n<-[1..m-1]] | m<-[2..]] where
  trip m n = PythTriple (a,b,c) cmp where
    [a,b,c] = qsort [m^2-n^2, 2*m*n, m^2+n^2]

pythTriples :: Integral a => [PythTriple a]
pythTriples = pythTriplesCmp (\t1 t2->compare (triple t1) (triple t2))

pythTriplesPerim :: Integral a => [PythTriple a]
pythTriplesPerim = pythTriplesCmp (\t1 t2->compare (perim t1) (perim t2))

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

isPrimeList :: Integral a => [(a, Bool)]
isPrimeList = (1,False):helper primes where
  helper (p:q:ps) = (p,True):[(n,False) | n<-[p+1..q-1]] ++ helper (q:ps)

composites :: Integral a => [a]
composites = [n | (n,isP)<-isPrimeList, not isP]

factors :: Integral a => a -> [a]
factors 1 = [1]
factors n = [i | i<-[1..fst (intSqrt n)], n `mod` i == 0]

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

(!!!) :: Integral a => [b] -> a -> b
(!!!) [] _ = error "Index out of bounds"
(!!!) (x:_) 0 = x
(!!!) (_:xs) n = xs !!! (n-1)

phiRatio :: Integral a => a -> Ratio a
phiRatio n= [phiRatio' i | i<-[1..]] !!! (n-1) where
  phiRatio' 1 = 1
  phiRatio' n
    | isEven n = if isEven halfn then 2*halfphi else halfphi
    | otherwise = (n % 1) * (1-(1 % p)) * (phiRatio n2 / (n2 % 1)) where
        halfn = quot n 2
        halfphi = phiRatio halfn
        n2 = divMax n p
        p = firstDivPrime n

phi :: Integral a => a -> a
phi = numerator . phiRatio

coeffRec :: Integral a => a -> a -> a
coeffRec _ 0 = 1
coeffRec n k = if n==k then 1 else coeffRec (n-1) (k-1) + coeffRec (n-1) k

coeffMul :: Integral a => a -> a -> a
coeffMul n k = numerator $ product [(n+1-i) % i | i<-[1..k]]

withMod :: Integral a => (a -> a -> a) -> a -> a -> a -> a
withMod _ 1 _ _ = 0
withMod f m a b = (f (a `mod` m) (b `mod` m)) `mod` m

addMod :: Integral a => a -> a -> a -> a
addMod = withMod (+)

mulMod :: Integral a => a -> a -> a -> a
mulMod = withMod (*)

subMod :: Integral a => a -> a -> a -> a
subMod = withMod (-)

expMod :: Integral a => a -> a -> a -> a
expMod 1 _ _ = 0
--expMod m a 2 = mulMod m a a
--expMod m a 1 = a `mod` m
expMod m _ 0 = 1
expMod m a b = if b < 0 then error "Negative exponent" else
                 if isEven b then
                  mulMod m halfb halfb else
                  mulMod m a (expMod m a $ b-1)
    where halfb = expMod m a (quot b 2)

sumMod :: Integral a => a -> [a] -> a
sumMod m arr = helper arr 0 where
  helper [] s = s
  helper (x:xs) s = helper xs $ ((x `mod` m) + s) `mod` m

productMod :: Integral a => a -> [a] -> a
productMod m arr = helper arr 0 where
  helper [] s = s
  helper (x:xs) s = helper xs $ ((x `mod` m) * s) `mod` m

partialSums :: Num a => [a] -> [a]
partialSums arr = helper 0 arr where
  helper s [] = [s]
  helper s (x:xs) = s:helper (x+s) xs

putOneLine :: [String] -> IO ()
putOneLine arr = mapM_ (\s->putStr('\r':s)>>hFlush stdout) arr

printOneLine :: Show a => [a] -> IO ()
printOneLine = putOneLine.(map show)

assert :: Bool -> IO ()
assert test = if test then putStr "" else error "Assertion failed!"
