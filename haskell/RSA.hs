
module RSA where

import Data.Char(ord,chr)
import EulerUtil
import System.Random

data Key = Key {keyVal :: Integer, modVal :: Integer} deriving (Show, Eq)
data KeyChain = KeyChain {private :: Key, public :: Key} deriving (Show, Eq)

strToInts :: String -> [Integer]
strToInts s = [toInteger $ ord c | c<-s]

intsToStr :: [Integer] -> String
intsToStr xs = [chr $ fromIntegral x | x <- xs]

runKey :: Key -> Integer -> Integer
runKey key x = expMod (modVal key) x (keyVal key)

encryptStr :: Key -> String -> [Integer]
encryptStr k s = map (runKey k) (strToInts s)

decryptStr :: Key -> [Integer] -> String
decryptStr k xs = intsToStr $ map (runKey k) xs

composites :: [Integer]
composites = 1:concat (helper primes) where
  helper (p:q:ps) = [p+1..q-1]:helper (q:ps)

areCoprime :: Integral a => a -> a -> Bool
areCoprime a b = (gcd a b) == 1

assertRet :: (a -> Bool) -> a -> a
assertRet test val = if test val then val else error "Assertion failed"

modInv :: Integral a => a -> a -> a
modInv a m = if areCoprime a m then
               assertRet test $ expMod m a ((phi m) - 1)
             else error "a must be coprime to m"
  where test x = mulMod m a x == 1

crackKey :: Key -> Key
crackKey (Key e n) = Key d n where
  [p,q] = distinctPrimeFactors n
  phiN = (p-1)*(q-1)
  d = modInv e phiN

getNth :: Integral b => [a] -> b -> a
getNth [] _ = error "Out of bounds"
getNth (x:xs) 0 = x
getNth (x:xs) n = if n < 0 then error "Negative index" else getNth xs (n-1)

calcN :: Integer -> Integer -> (Integer, Integer)
calcN n m = (n', phiN) where
  (p,q) = (primes `getNth` n, primes `getNth` m)
  n' = p * q
  phiN = (p-1)*(q-1)

makeKeyChain :: Integer -> Integer -> Integer -> KeyChain
makeKeyChain e n phiN = KeyChain {private = Key {keyVal = d, modVal = n},
                                  public = Key {keyVal = e, modVal = n}} where
  d = modInv e phiN

--Disgusting impure functions

randomChoice :: [a] -> IO a
randomChoice [] = error "Empty list to choose from"
randomChoice arr = do
  let len = length arr
  i <- randomRIO (0,len-1)
  return $ arr !! i

randomE :: Integer -> IO Integer
randomE phiN = search where
  search = do
    e <- randomRIO (2,phiN-1)
    if areCoprime e phiN then return e else search

generateKey :: Integer -> Integer -> IO KeyChain
generateKey n m = do
  let (n', phiN) = calcN n m
  e <- randomE phiN
  return $ makeKeyChain e n' phiN
  
  
