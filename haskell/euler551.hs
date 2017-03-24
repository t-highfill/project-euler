import EulerUtil

digSum n = helper 0 n where
    helper s 0 = s
    helper s m = helper (s+r) q where
        (q, r) = divMod m 10

arr :: [Integer]
arr = helper 1 where helper n = n:helper (n+digSum n)

main = printOneLine (take (10^15) arr) >> putStrLn ""