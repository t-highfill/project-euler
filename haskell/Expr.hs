
module Expr where

import Data.Ratio(numerator, denominator)

data Expr a = Var {identifier :: String} | Val a | Add (Expr a) (Expr a) | Mul (Expr a) (Expr a)
            | Neg (Expr a) | Div (Expr a) (Expr a) | Abs (Expr a)

instance (Eq a) => Eq (Expr a) where
    (Var i)==(Var j) = i==j
    (Val a)==(Val b) = a==b
    (Neg a)==(Neg b) = a==b
    (Abs a)==(Abs b) = a==b
    (Add a b)==(Add c d) = (a,b)==(c,d) || (a,b)==(d,c)
    (Mul a b)==(Mul c d) = (a,b)==(c,d) || (a,b)==(d,c)
    (Div a b)==(Div c d) = (a,b)==(c,d)
    _ == _ = False

mkDiv :: (Eq a, Num a) => Expr a -> Expr a -> Expr a
mkDiv (Val 1) (Div e1 e2) = mkDiv e2 e1
mkDiv e1 e2 = if e1 == e2 then Val 1 else Div e1 e2

exprShow2 c e1 e2 = '(':show e1 ++ c:show e2 ++ ")"

instance (Show a) => Show (Expr a) where
    show (Var i) = i
    show (Val x) = show x
    show (Neg e) = '-':show e
    show (Add e1 e2) = exprShow2 '+' e1 e2
    show (Mul e1 e2) = exprShow2 '*' e1 e2
    show (Div e1 e2) = exprShow2 '/' e1 e2
    show (Abs e) = '|':show e ++ "|"

instance (Num a, Eq a) => Num (Expr a) where
    (Val x) + (Val y) = Val (x+y)
    (Val 0) + e = e
    e + (Val 0) = e
    (Div a b) + (Div c d) = if b==d then mkDiv (a+c) b else Add (Div a b) (Div c d)
    e1 + e2 = Add e1 e2
    
    (Val x) * (Val y) = Val (x*y)
    (Val 0) * _ = Val 0
    _ * (Val 0) = Val 0
    (Val 1) * e = e
    e * (Val 1) = e
    (Div a b) * (Div c d)
                        | c==b = mkDiv a d
                        | a==d = mkDiv b c
                        | otherwise = mkDiv (a*c) (b*d)
    e1 * e2 = Mul e1 e2

    negate (Val x) = Val (-x)
    negate (Neg e) = e
    negate e = Neg e
    
    abs (Val x) = Val (abs x)
    abs (Abs e) = Abs e
    abs e = Abs e

    fromInteger = Val . fromInteger

    signum (Val x) = Val (signum x)
    signum e = error "This is unknown"

instance (Num a, Eq a) => Fractional (Expr a) where
    fromRational rat = (fromInteger $ numerator rat) / (fromInteger $ denominator rat)
    (/) = mkDiv
    recip e = (Val 1) / e

children :: Expr a -> [Expr a]
children (Add e1 e2) = [e1, e2]
children (Mul e1 e2) = [e1, e2]
children (Div e1 e2) = [e1, e2]
children (Neg e) = [e]
children (Abs e) = [e]
children _ = []

replaceExpr :: (Num a, Eq a) => Expr a -> Expr a -> Expr a -> Expr a
replaceExpr old new curr = if curr == old then new else cont curr where
    f = replaceExpr old new
    cont (Add e1 e2) = (f e1) + (f e2)
    cont (Mul e1 e2) = (f e1) * (f e2)
    cont (Div e1 e2) = mkDiv (f e1) (f e2)
    cont (Abs e) = abs (f e)
    cont (Neg e) = negate (f e)
    cont c = c

evalWith :: Num a => (a->a->a) -> [(String, a)] -> Expr a -> a
evalWith _ vars (Var i) = helper vars where
    helper [] = error $ "No value found for identifier " ++ i
    helper ((k,v):ks) = if i == k then v else helper ks
evalWith d vars (Add e1 e2) = evalWith d vars e1 + evalWith d vars e2
evalWith d vars (Mul e1 e2) = evalWith d vars e1 * evalWith d vars e2
evalWith d vars (Div e1 e2) = evalWith d vars e1 `d` evalWith d vars e2
evalWith d vars (Neg e) = negate $ evalWith d vars e
evalWith d vars (Abs e) = abs $ evalWith d vars e
evalWith _ _ (Val x) = x

evalWithQuot :: Integral a => [(String, a)] -> Expr a -> a
evalWithQuot = evalWith quot

evalWithDiv :: Integral a => [(String, a)] -> Expr a -> a
evalWithDiv = evalWith div

evalWithFrac :: Fractional a => [(String, a)] -> Expr a -> a
evalWithFrac = evalWith (/)
