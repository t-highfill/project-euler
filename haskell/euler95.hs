
properDivs :: Integer -> [Integer]
properDivs 1 = []

properDivs n = if d == 1 then [1] else
	   1:[i*d | i<-properDivs d2] where
	   	  d = head [x | x<-[2..quot n 2]++[1], n `mod` x == 0]
		  d2 = quot n d
