
import EulerUtil

arrangements m n = sum [coeffMul (slices-i*n) n * (m-i) | i<-[0..m-1]] where
  slices = m*n
