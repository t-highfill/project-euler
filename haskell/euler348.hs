
import EulerUtil (mergeMany, streaks)

squares :: [Integer]
squares = [n*n | n<-[1..]]

cubes :: [Integer]
cubes = [n^3 | n<-[1..]]

sqrCubes :: [Integer]
sqrCubes = helper [] cubes where
  helper seqs (c:n:rest) = mergeMany fronts ++ helper backs (n:rest) where
      breaks = map (span (<n)) $ [s+c | s<-squares]:seqs
      fronts = map fst breaks
      backs = map snd breaks

look4 :: Eq a => [a] -> [a]
look4 as = [a | (a, s)<-streaks as, s == 4]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome arr = arr == reverse arr

look4Palindrome :: (Eq a, Show a) => [a] -> [a]
look4Palindrome as = [a | a<-look4 as, isPalindrome (show a)]

main = print $ sum $ take 5 $ look4Palindrome sqrCubes
