
import Data.Ratio

concats :: [Rational] -> [[Rational]]
concats [] = []
concats [a] = [[a]]
concats (x:y:xs) = (concats $ (10*x+y):xs) ++ (map (x:) $ concats (y:xs))

mutations :: [Rational] -> [[Rational]]
mutations [] = []
mutations [a] = [[a]]
mutations (x:y:xs) = (concat [mutations ((f x y):xs) | f<-[(+),(-),(*),(/)]])
                     ++ (map (x:) $ mutations (y:xs))

reachables :: [Rational] -> [Rational]
reachables [] = []
reachables [a] = if a > 0 && denominator a == 1 then [a] else []
reachables arr = concatMap reachables $ filter (/=arr)
                 $ concatMap mutations (concats arr)

run digs = [d%1 | d<-digs]
