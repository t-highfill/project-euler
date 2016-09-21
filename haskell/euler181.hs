
--import quailfied Data.Set as Set

--data Ball = Black | White

--type Bag = Set.Set Ball

--makeBag b w = 

groups b w = if b+w<=1 then 1 else 1+(if b>=1 then groups (b-1) w)+(if w>=1 then groups b (w-1))

main = print $ groups 3 1
