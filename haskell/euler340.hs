
import EulerUtil

modMask = 10^9

crazy a b c = helper where
  add = addMod modMask
  helper n = if n > b then subMod modMask n c
    else (f.f.f.f) n where f x = helper (a `add` x)

sumCrazy a b c = sumMod modMask [f n | n<-[0..b]] where
  f = crazy a b c

main = print $ sumCrazy (21^7) (7^21) (12^7)
