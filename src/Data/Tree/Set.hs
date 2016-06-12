{-# LANGUAGE
    DeriveFoldable
  , DeriveGeneric
  , DeriveDataTypeable
  , FlexibleInstances
  , MultiParamTypeClasses
  #-}

module Data.Tree.Set where

import Prelude hiding (map, elem, filter)
import qualified Data.Set as Set
import qualified Data.Foldable as F
import qualified Data.Maybe as M
import Data.Semigroup
import Data.Semigroup.Foldable
import qualified Data.Set.Class as Sets
import Control.Monad
import qualified Data.Tree as T

import Data.Data
import GHC.Generics
import Control.DeepSeq
import Test.QuickCheck
import Test.QuickCheck.Instances


data SetTree a = SetTree
  { sNode     :: a
  , sChildren :: !(Set.Set (SetTree a))
  } deriving (Show, Eq, Ord, Foldable, Generic, Data, Typeable)

instance NFData a => NFData (SetTree a)

instance (Ord a, Arbitrary a) => Arbitrary (SetTree a) where
  arbitrary = SetTree <$> arbitrary <*> arbitrary

instance Foldable1 SetTree where
  fold1 (SetTree x xs) = F.foldr (\a acc -> sNode a <> acc) x xs

instance Ord a => Semigroup (SetTree a) where
  (SetTree _ xs) <> (SetTree y ys) = SetTree y (xs <> ys)

instance Sets.HasSize (SetTree a) where
  size = size

instance Sets.HasSingleton a (SetTree a) where
  singleton = singleton


-- * Query

-- | set-like alias for @isDescendantOf@.
elem :: Eq a => a -> SetTree a -> Bool
elem = isDescendantOf

elemPath :: Eq a => [a] -> SetTree a -> Bool
elemPath [] _ = True
elemPath (x:xs) (SetTree y ys) =
  (x == y) && getAny (F.foldMap (Any . elemPath xs) ys)

size :: SetTree a -> Int
size (SetTree _ xs) = 1 + getSum (F.foldMap (Sum . size) xs)

isChildOf :: Eq a => a -> SetTree a -> Bool
isChildOf x (SetTree _ ys) =
  getAny $ F.foldMap (Any . (x ==) . sNode) ys

isDescendantOf :: Eq a => a -> SetTree a -> Bool
isDescendantOf x (SetTree y ys) =
  (x == y) || getAny (F.foldMap (Any . isDescendantOf x) ys)

-- | Heirarchical analogue to subseteq.
isSubtreeOf :: Eq a => SetTree a -> SetTree a -> Bool
isSubtreeOf xss yss@(SetTree _ ys) =
  xss == yss || getAny (F.foldMap (Any . isSubtreeOf xss) ys)

-- | Bottom-up version
isSubtreeOf' :: Eq a => SetTree a -> SetTree a -> Bool
isSubtreeOf' xss yss@(SetTree _ ys) =
  getAny (F.foldMap (Any . isSubtreeOf' xss) ys) || xss == yss

isProperSubtreeOf :: Eq a => SetTree a -> SetTree a -> Bool
isProperSubtreeOf xss (SetTree _ ys) =
  getAny $ F.foldMap (Any . isSubtreeOf xss) ys

-- | Bottom-up version
isProperSubtreeOf' :: Eq a => SetTree a -> SetTree a -> Bool
isProperSubtreeOf' xss (SetTree _ ys) =
  getAny $ F.foldMap (Any . isSubtreeOf' xss) ys

eqHead :: Eq a => SetTree a -> SetTree a -> Bool
eqHead (SetTree x _) (SetTree y _) = x == y

-- * Construction

insertChild :: Ord a => SetTree a -> SetTree a -> SetTree a
insertChild x (SetTree y ys) = SetTree y $ Set.insert x ys

delete :: Eq a => a -> SetTree a -> Maybe (SetTree a)
delete x = filter (/= x)

singleton :: a -> SetTree a
singleton x = SetTree x Set.empty

-- * Filtering

filter :: Eq a => (a -> Bool) -> SetTree a -> Maybe (SetTree a)
filter p (SetTree x xs) = do
  guard $ p x
  return $ SetTree x $ Set.fromAscList $ M.mapMaybe (filter p) $ Set.toAscList xs

-- * Mapping

map :: Ord b => (a -> b) -> SetTree a -> SetTree b
map f (SetTree x xs) = SetTree (f x) $ Set.map (map f) xs

mapMaybe :: Eq b => (a -> Maybe b) -> SetTree a -> Maybe (SetTree b)
mapMaybe p (SetTree x xs) = do
  x' <- p x
  return $ SetTree x' $ Set.fromAscList $ M.mapMaybe (mapMaybe p) $ Set.toAscList xs


toTree :: SetTree a -> T.Tree a
toTree (SetTree x xs) = T.Node x $ toTree <$> Set.toList xs

fromTree :: Ord a => T.Tree a -> SetTree a
fromTree (T.Node x xs) = SetTree x . Set.fromList $ fromTree <$> xs
