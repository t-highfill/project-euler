
import EulerUtil
import Control.Concurrent.MVar
import Data.List.Split(divvy)
import Control.Concurrent.Async

digits :: Integral a => a -> [a]
digits n = if n < 10 then [n] else (digits n') ++ [d] where
  (n', d) = divMod n 10

fromDigits :: Integral a => [a] -> a
fromDigits digs = helper digs 0 where
  helper [] n = n
  helper (d:ds) n = helper ds $ n*10+d

allOdd :: Integral a => a -> Bool
allOdd n = all isOdd $ digits n

revNum :: Integral a => a -> a
revNum n = fromDigits $ reverse $ digits n

reversible :: Integral a => a -> Bool
reversible n = n `mod` 10 /= 0 && (allOdd $ (revNum n) + n)

doThread :: (Integral a, Show a) => [a] -> MVar a -> IO ()
doThread vals total = do
  let res = sum [1 | n<-vals, reversible n]
  putStrLn $ (show res) ++ "<--Partial result"
  tot <- takeMVar total
  putMVar total (tot+res)

main = do
  let limit = 10^9
  let threads = 4
  let segSize = limit `quot` threads
  let divvied = divvy segSize segSize [0..limit-1]
  total <- newMVar 0
  let doStuff seg = async $ doThread seg total
  ids <- mapM doStuff divvied
  mapM_ wait ids
  tot <- takeMVar total
  print tot
