
module Calculus where

import Data.Ratio

class CalcFunc a where
  evalAt :: Real b => a -> b -> b

class (CalcFunc a) => Deriv a where
  deriv :: CalcFunc b => a -> b

class (CalcFunc a) => Integ a where
  integ :: CalcFunc b => a -> b
  defInteg :: Real c => a -> c -> c -> c
  defInteg f a b = (evalAt f' b) - (evalAt f' a) where f' = integ f

-- instance (Real a) => CalcFunc (a) where
--   evalAt f _ = f

-- instance (Real a) => Deriv a where
--   deriv _ = 0

-- instance (Real a) => Integ a where
--   integ f = Polynomial [0, f]

data Polynomial a = Polynomial {coeffs :: [a]}

instance (Real a) => CalcFunc (Polynomial a) where
  evalAt poly x = evalAt' (coeffs poly) x where
    evalAt' [] _ = error "Empty Polynomial!"
    evalAt' [c] _ = c
    evalAt' (c:cs) y = c+(evalAt' [y*c' | c'<-cs] y)

instance (Real a) => Deriv (Polynomial a) where
  deriv poly = Polynomial (helper $ coeffs poly) where
    helper (_:cs) = zipWith (*) cs [1..]

instance (Real a) => Integ (Polynomial a) where
  integ (Polynomial cs) = Polynomial (helper $ map toRational cs) where
    helper cRats = 0:zipWith (*) cRats [1 % i | i<-[1..]]
