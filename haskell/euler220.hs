
data Dir = North | South | East | West

left :: Dir -> Dir
left North = West
left West = South
left South = East
left East = North

right :: Dir -> Dir
right North = East
right East = South
right South = West
right West = North

rotate :: Char -> Dir -> Dir
rotate 'R' = right
rotate 'L' = left

move :: Dir -> (Integer, Integer) -> (Integer, Integer)
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East (x,y) = (x+1,y)
move West (x,y) = (x-1,y)

code :: Int -> String
code n = helper n "Fa" where
  helper 0 str = str
  helper n [] = []
  helper n ('a':cs) = helper (n-1) "aRbFR" ++ helper n cs
  helper n ('b':cs) = helper (n-1) "LFaLb" ++ helper n cs
  helper n (c:cs) = c:helper n cs

walk n steps = helper steps (0,0) North (code n) where
  helper 0 pos _ _ = pos
  helper s pos d ('F':cs) = helper (s-1) (move d pos) d cs
  helper s pos d (c:cs) = if c `elem` "LR" then helper s pos (rotate c d) cs
                          else helper s pos d cs

main = print $ walk 50 (10^12)
