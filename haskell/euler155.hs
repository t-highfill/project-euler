
import Data.Ratio
import Data.List
import EulerUtil

type CapType = Ratio Int

data Capacitor = Single CapType | Parallel [Capacitor] | Series [Capacitor] deriving (Show, Eq)

capVal = 60
basic = Single capVal

prevHeadTails :: [a] -> [([a],a,[a])]
prevHeadTails arr = helper [] arr where
    helper prev [] = []
    helper prev (x:xs) = (prev,x,xs):helper (prev++[x]) xs

capacitance :: Capacitor -> CapType
capacitance (Single c) = c
capacitance (Parallel cs) = sum $ map capacitance cs
capacitance (Series cs) = 1/(sum $ map ((1/).capacitance) cs)

circuits :: [[Capacitor]]
circuits = circuits' [basic] where
    circuits' prev = prev:circuits' (concatMap integ prev) where
        integ (Single c) = [con [basic,Single c] | con<-[Parallel,Series]]
        integ (Parallel cs) = integ' Parallel cs
        integ (Series cs) = integ' Series cs
        integ' con cs = (con $ basic:cs)
                        :[con $ p ++ newC:t | (p,c,t)<-prevHeadTails cs, newC<-integ c]

main = do
    printOneLine $ partialSums [1 | circs<-take 18 circuits, _<-nub $ map capacitance circs]
    putStrLn "\nDone!"