
import Numeric (showHex)
import Data.Char (toUpper)

maxlen :: Integer
maxlen = 16

possibles = sum [1 | c<-"23456789BCDEF"]

numbers :: Integer -> (Bool, Bool, Bool) -> Integer
numbers len (zero,one,a) = if len == maxlen then this else sum [this, zero', one', a', rest] where
        this = if all id [zero, one, a] then 1 else 0
        zero' = if len > 0 then numbers (len+1) (True, one, a) else 0
        one' = numbers (len+1) (zero, True, a)
        a' = numbers (len+1) (zero, one, True)
        rest = possibles * (numbers (len+1) (zero, one, a))

result :: Integer
result = numbers 0 (False, False, False)

main :: IO ()
main = putStrLn $ map toUpper (showHex result "")

