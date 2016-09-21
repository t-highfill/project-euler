
squares :: [Integer]
squares = [i*i | i<-[1..]]

isSquare :: Integer -> Bool
isSquare n = search n squares where
	 search n' (x:xs) = x == n' || (n' > x && search n' xs)

limit = 10^9

determinant a i = s*(s-a)^2*(s-a-i) where s = quot (3*a+i) 2

testNum a = sum [1 | i<-[1,-1], 3*a+i < limit,
	    	    isSquare $ determinant a i]

main = print $ sum [testNum n | n<-[3,5..quot (limit + 1) 3]]
