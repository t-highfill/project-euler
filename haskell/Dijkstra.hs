
module Dijkstra where

import Data.Set(Set)
import qualified Data.Set as Set

data Path a w = Path {orig :: a, dest :: a, weight :: w} deriving Show, Eq
data Graph a w = Graph {nodes :: Set a, paths :: Set (Path a w)} deriving Show, Eq

instance Ord w => Ord (Path a w) where
  compare p1 p2 = compare (weight p1) (weight p2)

infinity = 1/0

neighbors :: Eq a => Graph a w -> a -> Set a
neighbors g n = Set.filter (\p->orig p == n) $ paths g

shortestPath :: (Num w, Eq a) => Graph a w -> a -> a -> [a]
shortestPath g start finish = helper $ Set.fromList q' where
  q' = (Path [start] [start] 0):[Path [n] [] infinity | n<-elems (nodes g), n/=start]
  helper q = if null q then [] else if u == finish then pth else helper newQ where
    (Path [u] pth wt) = Set.findMin q
    newQ = Set. --TODO: Finish
