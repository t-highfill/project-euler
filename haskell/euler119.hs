
import EulerUtil(mergeInf)
import System.IO

crush :: Eq a => [(a,b)] -> [(a,[b])]
crush [] = []
crush ((a,b):ps) = helper (a,[b]) ps where
  helper last [] = [last]
  helper (n,arr) ((x,y):rest) = if n==x then helper (n,arr++[y]) rest
                                else (n,arr):helper (x,[y]) rest

perfectPowers = crush $ mergeInf [helper a $ a^2 | a<-[2..]] where
  helper a last = (last,a):helper a (a*last)

elemAsc :: Ord a => a -> [a] -> Bool
elemAsc _ [] = False
elemAsc n (x:xs) = n==x || (n > x && elemAsc n xs)

digitSum :: (Integral a, Read a, Show a) => a -> a
digitSum n = sum [read [c] | c<-show n]

dps = [n | (n,as)<-perfectPowers, digitSum n `elemAsc` as]

showOneLine arr = mapM_ put arr where
  put x = do
    putStr $ show x ++ "\r"
    hFlush stdout

main = do
  showOneLine $ take 30 $ zip [1..] dps
  putStrLn "\nDone!"
