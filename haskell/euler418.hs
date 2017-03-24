
import EulerUtil

facFactors n = mergeMany $ map pfactors $ takeWhile (\x->original x<=n) primeFacs

result n = (a'+b'+c', a', b', c') where
  (a',b',c') = helper 1 1 1 $ reverse $ facFactors n
  helper a b c [] = (a,b,c)
  helper a b c (f:fs) = if f*a <= b then helper (f*a) b c fs
                        else if f*b <= c then helper a (f*b) c fs
                             else helper a b (f*c) fs
