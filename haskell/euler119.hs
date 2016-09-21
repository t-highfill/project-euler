

digitsum :: Integer -> Integer
digitsum n = sum [read (c:"") :: Integer | c<-show n]

--isInt :: (RealFrac a) => a -> Bool
isInt f = (floor f) == (ceiling f)

--logBaseInt :: (Integral a, Rational b) => a -> a -> b
logBaseInt base x = logBase (fromIntegral base) (fromIntegral x) 

getExp n = logBaseInt (digitsum n) n

isInteresting :: Integer -> Bool
isInteresting n = if digitsum n == 1 then False else
	      isInt $ getExp n
main = do
     print $ take 10 [(n,getExp n) | n<-[10..], isInteresting n]