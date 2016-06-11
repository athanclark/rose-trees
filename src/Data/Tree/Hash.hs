{-# LANGUAGE
    DeriveFoldable
  , DeriveGeneric
  , DeriveDataTypeable
  , FlexibleInstances
  , MultiParamTypeClasses
  #-}

module Data.Tree.Hash where

import Prelude hiding (map, elem, filter)
import qualified Data.HashSet  as HS
import qualified Data.Foldable as F
import qualified Data.Maybe    as M
import qualified Data.Set.Class as Sets
import Data.Hashable
import Data.Semigroup
import Data.Semigroup.Foldable
import Control.Monad

import Data.Data
import GHC.Generics
import Control.DeepSeq


data HashTree a = HashTree
  { sNode     :: !a
  , sChildren :: !(HS.HashSet (HashTree a))
  } deriving (Show, Eq, Foldable, Generic, Data, Typeable)

instance Hashable a => Hashable (HashTree a) where
  hashWithSalt salt (HashTree x xs) =
    salt `hashWithSalt` x `hashWithSalt` xs

instance NFData a => NFData (HashTree a)

instance Foldable1 HashTree where
  fold1 (HashTree x xs) = F.foldr (\a acc -> sNode a <> acc) x xs

instance Sets.HasSize (HashTree a) where
  size = size

instance Sets.HasSingleton a (HashTree a) where
  singleton = singleton

instance (Eq a, Hashable a) => Semigroup (HashTree a) where
  (HashTree _ xs) <> (HashTree y ys) = HashTree y (xs <> ys)


-- * Query

-- | set-like alias for @isDescendantOf@.
elem :: Eq a => a -> HashTree a -> Bool
elem = isDescendantOf

size :: HashTree a -> Int
size (HashTree _ xs) = 1 + getSum (F.foldMap (Sum . size) xs)

isChildOf :: Eq a => a -> HashTree a -> Bool
isChildOf x (HashTree _ ys) =
  getAny $ F.foldMap (Any . (x ==) . sNode) ys

isDescendantOf :: Eq a => a -> HashTree a -> Bool
isDescendantOf x (HashTree y ys) =
  (x == y) || getAny (F.foldMap (Any . isDescendantOf x) ys)

-- | Heirarchical analogue to subseteq.
isSubtreeOf :: (Eq a, Hashable a) => HashTree a -> HashTree a -> Bool
isSubtreeOf xss yss@(HashTree _ ys) =
  xss == yss || getAny (F.foldMap (Any . isSubtreeOf xss) ys)

-- | Bottom-up version
isSubtreeOf' :: (Eq a, Hashable a) => HashTree a -> HashTree a -> Bool
isSubtreeOf' xss yss@(HashTree _ ys) =
  getAny (F.foldMap (Any . isSubtreeOf' xss) ys) || xss == yss

isProperSubtreeOf :: (Eq a, Hashable a) => HashTree a -> HashTree a -> Bool
isProperSubtreeOf xss (HashTree _ ys) =
  getAny $ F.foldMap (Any . isSubtreeOf xss) ys

-- | Bottom-up version
isProperSubtreeOf' :: (Eq a, Hashable a) => HashTree a -> HashTree a -> Bool
isProperSubtreeOf' xss (HashTree _ ys) =
  getAny $ F.foldMap (Any . isSubtreeOf' xss) ys

eqHead :: Eq a => HashTree a -> HashTree a -> Bool
eqHead (HashTree x _) (HashTree y _) = x == y

-- * Construction

insertChild :: (Eq a, Hashable a) => HashTree a -> HashTree a -> HashTree a
insertChild x (HashTree y ys) = HashTree y $ HS.insert x ys

delete :: (Eq a, Hashable a) => a -> HashTree a -> Maybe (HashTree a)
delete x = filter (/= x)

singleton :: a -> HashTree a
singleton x = HashTree x HS.empty

-- * Filtering

filter :: (Eq a, Hashable a) => (a -> Bool) -> HashTree a -> Maybe (HashTree a)
filter p (HashTree x xs) = do
  guard $ p x
  pure . HashTree x . HS.fromList . M.mapMaybe (filter p)
                       . HS.toList $ xs

-- * Mapping

map :: (Eq b, Hashable b) => (a -> b) -> HashTree a -> HashTree b
map f (HashTree x xs) = HashTree (f x) $ HS.map (map f) xs

mapMaybe :: (Eq b, Hashable b) => (a -> Maybe b) -> HashTree a -> Maybe (HashTree b)
mapMaybe p (HashTree x xs) = do
  x' <- p x
  pure . HashTree x' . HS.fromList . M.mapMaybe (mapMaybe p)
                        . HS.toList $ xs
