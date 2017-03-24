
import EulerUtil
import Data.List(genericTake)

modNum :: Integral a => a
modNum = 14^8

repl :: Integral a => a -> b -> [b]
repl n = (genericTake n) . repeat

knuth :: Integral a => a -> a -> a -> a
knuth 0 _ _ = error "Arrow count must be positive"
knuth 1 a b = expMod modNum a b
knuth _ _ 1 = 1
knuth n a b = foldr1 (knuth (n-1)) $ repl b a

ack :: Integral a => a -> a -> a
ack 0 n = addMod modNum n 1
ack 1 n = addMod modNum n 2
ack 2 n = addMod modNum (2*n) 3
ack m n = (knuth (m-2) 2 (n+3)) - 3

main = print $ sumMod modNum [ack n n | n<-[0..6]]
