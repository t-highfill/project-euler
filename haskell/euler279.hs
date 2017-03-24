
import EulerUtil

perim :: Num a => PythTriple a -> a
perim (PythTriple (a,b,c) _) = a+b+c

perimTriples :: [PythTriple Integer]
perimTriples = pythTriplesCmp (\t1 t2->compare (perim t1) (perim t2))

limit :: Integer
limit = 10^8

trips :: [Integer]
trips = takeWhile (<limit) $ map perim perimTriples

intersection :: Ord a => [a] -> [a] -> [a]
intersection _ [] = []
intersection [] _ = []
intersection (x:xs) (y:ys)
  | x==y      = x:intersection xs ys
  | x<y       = intersection xs (y:ys)
  | otherwise = intersection (x:xs) ys

squares :: [Integer]
squares = [n*n | n<-[1..]]

sixtySides :: [Integer]
sixtySides = intersection squares $ mergeInf [[a^2+b^2-a*b | b <-[1..a]]
                                             | a<-[1..]]

result :: Integer
result = sum [1 | _<-trips]

main = print result
