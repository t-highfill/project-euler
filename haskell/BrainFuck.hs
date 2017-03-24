
module BrainFuck where

data BrainFuck = Incr | Decr | Left | Right | Read | Print
				| Loop {children :: [BrainFuck]} deriving (Show, Eq)

compileBF :: String -> [BrainFuck]
compileBF [] = []
compileBF ('+':cs) = Incr:compileBF cs
compileBF ('-':cs) = Decr:compileBF cs
compileBF ('>':cs) = Left:compileBF cs
compileBF ('<':cs) = Right:compileBF cs
compileBF (',':cs) = Read:compileBF cs
compileBF ('.':cs) = Print:compileBF cs
compileBF ('[':cs) = Loop (compileBF left):compileBF right where
	(left, right) = findMatch 0 cs []
	findMatch _ [] _ = error "No matching closing brace!"
	findMatch 0 (']':cs) buf = (buf, cs)
	findMatch n ('[':cs) buf = findMatch (n+1) cs (buf++"[")
	findMatch n (']':cs) buf = findMatch (n-1) cs (buf++"]")
	findMatch n (c:cs) buf = findMatch n cs (buf++[c])
compileBF (']':cs) = error "No matching opening brace!"
compileBF (_:cs) = compileBF cs

data BFState a = BFState {pointer :: Integer, tape :: [a], loopStack :: [BrainFuck]} deriving Eq

initState :: Num a => BFState a
initState = BFState 0 (repeat 0) []

changeTape :: (a -> a) -> BFState a -> BFState a
changeTape f state = BFState idx newTape (loopStack state) where
	idx = pointer state
	newTape = helper idx (tape state)
	helper 0 (n:ns) = (f n):ns
	helper i (_:ns) = helper (i-1) ns

readTape :: Integral a => BFState b -> b
readTape state = helper (pointer state) (tape state) where
	helper 0 = head
	helper _ [] = error "Out of space"
	helper n (x:xs) = helper (n-1) xs

