fibs :: [Integer]
fibs = 1:(fibhelp 1 1) where fibhelp prev curr = curr:(fibhelp curr (prev+curr))

last9 :: Integer -> String
last9 x = show $ x `mod` 10^9

intdiv a b = floor $ (fromIntegral a) / (fromIntegral b)

diglen :: Integer -> Integer
diglen 0 = 1
diglen 1 = 1
diglen n = if n<0 then diglen (abs n) else toInteger $ length (show n)

first9 :: Integer -> String
first9 n = take 9 (show n)

ispan :: String -> Bool
ispan n = all (\x -> x `elem` n) "123456789"

main = print $ head [idx | (idx,x)<-(zip [1..] fibs), ispan (last9 x), ispan (first9 x)]