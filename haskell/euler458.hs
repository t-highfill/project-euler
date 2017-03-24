
import EulerUtil

result :: Integer -> Integer -> Integer
result m n = mulMod m front $ addMod m inner fac6 where
  front = expMod m 7 (n-6)
  inner = expMod m 7 6
  fac6 = (-1) * product [1..6] * (n-6)

main = print $ result (10^9) (10^12)
