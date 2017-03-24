
import EulerUtil(mergeInf, streaks)

generated = mergeInf [[a2+a3*b+b^2 | b<-[1..a-1]] | a<-[2..], let a2=a^2;a3=3*a]

lilF n r = sum [1 | (_,s)<-streaks nums, s == r] where
  nums = takeWhile (<=n) generated

main = print $ lilF (10^15) 40
