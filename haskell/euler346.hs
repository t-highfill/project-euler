
import EulerUtil
import Numeric(readInt)

reps :: [String]
reps = helper "1" where helper s = s:helper ('1':s)

readRep :: Integral a => a -> String -> a
readRep base = fst . head . readInt base (\'1'->True) (\'1'->1)

repNums :: [Integer]
repNums = nostrk $ 1:mergeInf [[readRep b r | b<-[2..]] | r<-tail $ tail reps] where
  nostrk (x:y:xs) = if x==y then nostrk (y:xs) else x:nostrk (y:xs)

sRepSum :: Integer -> Integer
sRepSum lim = sum $ takeWhile (<lim) repNums

main :: IO ()
main = print $ sRepSum (10^12)
