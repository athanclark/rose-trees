module Data.Tree.Set where

import Prelude hiding (map)
import qualified Data.Set as Set
import qualified Data.Foldable as F
import Data.Monoid



data SetTree a = SetTree
  { sNode     :: a
  , sChildren :: Set.Set (SetTree a)
  } deriving (Show, Eq, Ord)

map :: Ord b => (a -> b) -> SetTree a -> SetTree b
map f (SetTree x xs) = SetTree (f x) $ Set.map (map f) xs

elem :: Eq a => a -> SetTree a -> Bool
elem x (SetTree y ys) | x == y = True
                      | otherwise = getAny $ F.foldMap (Any . Data.Tree.Set.elem x) ys
