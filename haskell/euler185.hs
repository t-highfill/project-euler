
import Data.List(sort, dropWhile)

guess :: [([Int], Int)] -> Int -> [([Int], Int)]
guess [] _ = []
guess (([],_):_) _ = []
guess (((d:ds),c):rest) g = (ds,c-i):guess rest g where
  i = if d==g then 1 else 0

verify :: [([Int], Int)] -> Bool
verify ((arr,c):rest) = null arr && c == 0 && (null rest || verify rest)

solve :: [([Int], Int)] -> [Int]
solve clues' = helper [] clues' where
  helper _ [] = []
  helper prev clues = if null first then
                        (if verify clues then prev else [])
                      else head $ dropWhile null tries where
    first = fst $ head clues
    heads = map (head . fst) clues
    corrs = map snd clues
    pairs = reverse $ sort [(product [c | (d',c)<-zip heads corrs, d'==d], d)
                           | d<-[0..9]]
    tries = [helper (prev++[d]) $ guess clues d | (wt,d)<-pairs, wt>0]

test = [([9,0,3,4,2], 2),
        ([7,0,7,9,4], 0),
        ([3,9,4,5,8], 2),
        ([3,4,1,0,9], 1),
        ([5,1,5,4,5], 2),
        ([1,2,5,3,1], 1)]

main = do
  print $ solve test
