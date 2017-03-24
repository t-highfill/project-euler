
data State = State {beenLate :: Bool, absentStreak :: Int} 

countStrs :: Int -> State -> Integer
countStrs 0 _ = 1
countStrs rem (State l as) = if as >= 3 then 0
                             else sum [onTime, late, absent] where
  onTime = countStrs (rem-1) $ State l 0
  absent = countStrs (rem-1) $ State l (as+1)
  late = if l then 0 else countStrs (rem-1) $ State True 0

main = print $ countStrs 4 (State False 0)
