
import EulerUtil((!!!))

fibHelper :: (a -> a -> a) -> a -> a -> [a]
fibHelper f = helper where
  helper a b = a:helper b (f a b)

fibs = fibHelper (+) 1 1

fibStrings = fibHelper (++) "A" "B"

digit :: String -> String -> Integer -> Integer
digit a b n = helper $ head [s | (s,l)<-zip fibStrings fibs, l*100 >= n] where
  helper fibStr = helper2 r $ fibStr !!! q where
    (q,r) = divMod n 100
  helper2 r 'A' = read $ (a !!! r):""
  helper2 r 'B' = read $ (b !!! r):""

main = do
  let a = "14159265358979323846264338327950288419716939937510"++
        "58209749445923078164062862089986280348253421170679"
  let b = "82148086513282306647093844609550582231725359408128"++
        "48111745028410270193852110555964462294895493038196"
  let digs = [digit a b $ (127+19*n)*(7^n) | n<-[0..17]]
  mapM_ print digs
  putStrLn $ concatMap show $ reverse digs
