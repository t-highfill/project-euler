
import EulerUtil hiding (primes)
import Data.Numbers.Primes
import System.IO

squareFrees :: [Integer]
squareFrees = helper 1 primes where
  helper n ps = n:mergeInf [helper (n*p) ps | (p,ps)<-headTails ps]

partialSums = helper 0 where
  helper s [] = [s]
  helper s (x:xs) = s:helper (x+s) xs

printOneLine arr = mapM_ putFlush $ map ((++"\r").show) arr where
  putFlush s = do
    putStr s
    hFlush stdout

main = do
  printOneLine $ partialSums [1 | _<-takeWhile (<(2^50)) squareFrees]
  putStrLn "\nDone!"
