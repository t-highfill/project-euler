
import EulerUtil

admissibles = tail $ helper 1 primes where
  helper n (p:ps) = n:mergeUnique (helper (n*p) (p:ps)) (helper (n*p) ps)

limit = 10^9

withPF = helper (takeWhile (<limit) admissibles) primes 0 where
  helper [] _ _ = []
  helper (n:ns) ps last = front ++ helper ns ps' m where
    front = if m/=last then [(n,m)] else []
    ps' = dropWhile (<=n+1) ps
    m = head ps'

result = sum [m | (n,m)<-withPF]

main = print result
