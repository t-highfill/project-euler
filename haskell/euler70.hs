
import Data.List(sort)
import EulerUtil(totients)
import Data.Ratio
import Control.Concurrent
import Control.Concurrent.MVar

isPermute a b = l1 == l2 && (sort s1) == (sort s2) where
  [s1, s2] = map show [a,b]
  [l1, l2] = map length [s1, s2]

split :: Int -> [a] -> [[a]]
split n arr = zipWith (:) front $ split n back ++ repeat [] where
  (front, back) = splitAt n arr

threadedFold :: Int -> ([a]->b) -> [a] -> IO [b]
threadedFold ts f arr = do
  mvars <- sequence [newEmptyMVar | _<-[1..ts]]
  mapM_ forkIO [putMVar mv (f a) | (mv, a)<-zip mvars $ split ts arr]
  mapM takeMVar mvars

main = do
  let threads = 4
  mins <- threadedFold threads minimum pairs
  print $ minimum mins where
    pairs = [(n%pn, n) | (n,pn)<-ts, pn/=n-1, isPermute n pn]
    ts = takeWhile ((<10^7).snd) $ tail totients
