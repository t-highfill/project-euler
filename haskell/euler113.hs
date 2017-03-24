
countNums :: (Num a, Eq a) => (a -> [a]) -> a -> a -> a
countNums _ 0   _ = 1
countNums f len d = 1 + sum [countNums f (len-1) d' | d'<-f d]

countInc = countNums (\d->[d..9])

countDec = countNums (\d->[0..d])

countRep = countNums (\d->[d])

countAll len d = (countInc len d) + (countDec len d) - (countRep len d)

result :: Integer -> Integer
result len = sum [countAll len d | d<-[1..9]]

main = print $ result 99
