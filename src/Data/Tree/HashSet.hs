{-# LANGUAGE
    DeriveFoldable
  , DeriveGeneric
  , DeriveDataTypeable
  , FlexibleInstances
  , MultiParamTypeClasses
  #-}

module Data.Tree.HashSet where

import Prelude hiding (map, elem, filter)
import qualified Data.HashSet  as HS
import qualified Data.Foldable as F
import qualified Data.Maybe    as M
import Data.Hashable
import Data.Semigroup
import Data.Semigroup.Foldable
import Control.Monad

import Data.Data
import GHC.Generics
import Control.DeepSeq


data HashSetTree a = HashSetTree
  { sNode     :: !a
  , sChildren :: !(HS.HashSet (HashSetTree a))
  } deriving (Show, Eq, Foldable, Generic, Data, Typeable)

instance Hashable a => Hashable (HashSetTree a) where
  hashWithSalt salt (HashSetTree x xs) =
    salt `hashWithSalt` x `hashWithSalt` xs

instance NFData a => NFData (HashSetTree a)

instance Foldable1 HashSetTree where
  fold1 (HashSetTree x xs) = F.foldr (\a acc -> sNode a <> acc) x xs


-- * Query

-- | set-like alias for @isDescendantOf@.
elem :: Eq a => a -> HashSetTree a -> Bool
elem = isDescendantOf

size :: HashSetTree a -> Int
size (HashSetTree _ xs) = 1 + getSum (F.foldMap (Sum . size) xs)

isChildOf :: Eq a => a -> HashSetTree a -> Bool
isChildOf x (HashSetTree _ ys) =
  getAny $ F.foldMap (Any . (x ==) . sNode) ys

isDescendantOf :: Eq a => a -> HashSetTree a -> Bool
isDescendantOf x (HashSetTree y ys) =
  (x == y) || getAny (F.foldMap (Any . isDescendantOf x) ys)

-- | Heirarchical analogue to subseteq.
isSubtreeOf :: (Eq a, Hashable a) => HashSetTree a -> HashSetTree a -> Bool
isSubtreeOf xss yss@(HashSetTree _ ys) =
  xss == yss || getAny (F.foldMap (Any . isSubtreeOf xss) ys)

-- | Bottom-up version
isSubtreeOf' :: (Eq a, Hashable a) => HashSetTree a -> HashSetTree a -> Bool
isSubtreeOf' xss yss@(HashSetTree _ ys) =
  getAny (F.foldMap (Any . isSubtreeOf' xss) ys) || xss == yss

isProperSubtreeOf :: (Eq a, Hashable a) => HashSetTree a -> HashSetTree a -> Bool
isProperSubtreeOf xss (HashSetTree _ ys) =
  getAny $ F.foldMap (Any . isSubtreeOf xss) ys

-- | Bottom-up version
isProperSubtreeOf' :: (Eq a, Hashable a) => HashSetTree a -> HashSetTree a -> Bool
isProperSubtreeOf' xss (HashSetTree _ ys) =
  getAny $ F.foldMap (Any . isSubtreeOf' xss) ys

eqHead :: Eq a => HashSetTree a -> HashSetTree a -> Bool
eqHead (HashSetTree x _) (HashSetTree y _) = x == y

-- * Construction

insertChild :: (Eq a, Hashable a) => HashSetTree a -> HashSetTree a -> HashSetTree a
insertChild x (HashSetTree y ys) = HashSetTree y $ HS.insert x ys

delete :: (Eq a, Hashable a) => a -> HashSetTree a -> Maybe (HashSetTree a)
delete x = filter (/= x)

singleton :: a -> HashSetTree a
singleton x = HashSetTree x HS.empty

-- * Filtering

filter :: (Eq a, Hashable a) => (a -> Bool) -> HashSetTree a -> Maybe (HashSetTree a)
filter p (HashSetTree x xs) = do
  guard $ p x
  pure . HashSetTree x . HS.fromList . M.mapMaybe (filter p)
                       . HS.toList $ xs

-- * Mapping

map :: (Eq b, Hashable b) => (a -> b) -> HashSetTree a -> HashSetTree b
map f (HashSetTree x xs) = HashSetTree (f x) $ HS.map (map f) xs

mapMaybe :: (Eq b, Hashable b) => (a -> Maybe b) -> HashSetTree a -> Maybe (HashSetTree b)
mapMaybe p (HashSetTree x xs) = do
  x' <- p x
  pure . HashSetTree x' . HS.fromList . M.mapMaybe (mapMaybe p)
                        . HS.toList $ xs
