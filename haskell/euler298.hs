
import System.Random

helper :: ([Int]->Int->[Int]) -> [Int] -> [Int] -> Integer -> Integer
helper _ [] _ score = score
helper insert (n:ns) mem score = helper insert ns (insert mem n) newscore where
  newscore = score + (if n `elem` mem then 1 else 0)

larry :: [Int] -> Integer
larry nums = helper insert nums [] 0 where
  insert mem n = n:[m | m<-take 4 mem, m/=n]

robin :: [Int] -> Integer
robin nums = helper insert nums [] 0 where
  insert mem n = if n `elem` mem then mem else n:(take 4 mem)

scoreDiff :: [Int] -> Integer
scoreDiff nums = abs $ (larry nums) - (robin nums)

test :: IO Integer
test = do
  nums <- getStdRandom (randomRs (1,10))
  return $ scoreDiff (take 50 nums)

main = testhelper 100 0 0 where
  testhelper 0 _ _ = return ()
  testhelper itrs sm cnt = do
    res <- test
    let sm' = sm+res
        cnt' = cnt+1
    print $ (fromIntegral sm') / (fromIntegral cnt')
    testhelper (itrs-1) sm' cnt'
