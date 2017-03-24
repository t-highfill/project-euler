
import EulerUtil

perim :: Num a => PythTriple a -> a
perim (PythTriple (a,b,c) _) = a+b+c

perimTriples :: [PythTriple Integer]
perimTriples = pythTriplesCmp (\t1 t2->compare (perim t1) (perim t2))

limit :: Integer
limit = 1500000

perims :: [Integer]
perims = takeWhile (<=limit) $ map perim perimTriples

result = sum [1 | (n, i)<-streaks perims, i==1]

main = print result
