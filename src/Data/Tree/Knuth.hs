{-# LANGUAGE
    DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , DeriveGeneric
  , DeriveDataTypeable
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , FlexibleInstances
  #-}

-- |
-- Module      : Data.Tree.Knuth
-- Copyright   : (c) 2014, 2015 Athan Clark
--
-- License     : BSD-style
-- Maintainer  : athan.clark@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- An implementation of
-- <https://en.wikipedia.org/wiki/Left-child_right-sibling_binary_tree left-child, right-sibling binary trees>.

module Data.Tree.Knuth where

import qualified Data.Tree.Knuth.Forest as KF

import Data.Semigroup
import Data.Foldable as F
import Data.Maybe
import qualified Data.Set.Class as Sets
import Control.Applicative
import Control.Monad
import Control.DeepSeq

import Data.Data
import Data.Typeable
import GHC.Generics
import Test.QuickCheck


newtype KnuthTree a = KnuthTree {
  unKnuthTree :: (a, KF.KnuthForest a)
  } deriving (Show, Eq, Functor, Foldable, Traversable, Generic, Data, Typeable)

instance Arbitrary a => Arbitrary (KnuthTree a) where
  arbitrary = do
    x  <- arbitrary
    xs <- arbitrary
    return $ KnuthTree (x,xs)

firstTree :: KF.KnuthForest a -> Maybe (KnuthTree a)
firstTree KF.Nil = Nothing
firstTree (KF.Fork x xc _) = Just $ KnuthTree (x,xc)


instance Applicative KnuthTree where
  pure x = KnuthTree (x,KF.Nil)
  (KnuthTree (f,fs)) <*> (KnuthTree (x,xs)) = KnuthTree (f x,fs <*> xs)

instance Monad KnuthTree where
  return x = KnuthTree (x,KF.Nil)
  (KnuthTree (x,xs)) >>= f =
    let (KnuthTree (y,_)) = f x
    in KnuthTree (y,xs >>= (snd . unKnuthTree . f))

instance Semigroup (KnuthTree a) where
  (<>) = union

instance Sets.HasSize (KnuthTree a) where
  size = size

instance Sets.HasSingleton a (KnuthTree a) where
  singleton = singleton

instance Sets.HasUnion (KnuthTree a) where
  union = union

-- ** Query
size :: KnuthTree a -> Int
size (KnuthTree (_,xs)) = 1 + KF.size xs

elem :: Eq a => a -> KnuthTree a -> Bool
elem x (KnuthTree (y,ys)) = x == y || KF.elem x ys

isSubtreeOf :: Eq a => KnuthTree a -> KnuthTree a -> Bool
isSubtreeOf xss yss@(KnuthTree (_,ys)) = xss == yss || go ys
  where
    go KF.Nil = False
    go zss@(KF.Fork x xc xs) = xss == fromJust (firstTree zss) || go xs || go xc

-- | Bottom-up depth-first
isSubtreeOf' :: Eq a => KnuthTree a -> KnuthTree a -> Bool
isSubtreeOf' xss yss@(KnuthTree (_,ys)) = go ys || xss == yss
  where
    go KF.Nil = False
    go zss@(KF.Fork x xc xs) = go xc || go xs || xss == fromJust (firstTree zss)

isProperSubtreeOf :: Eq a => KnuthTree a -> KnuthTree a -> Bool
isProperSubtreeOf xss (KnuthTree (_,ys)) = go ys
  where
    go KF.Nil = False
    go zss@(KF.Fork x xc xs) = xss == fromJust (firstTree zss) || go xs || go xc

-- | Bottom-up depth-first
isProperSubtreeOf' :: Eq a => KnuthTree a -> KnuthTree a -> Bool
isProperSubtreeOf' xss (KnuthTree (_,ys)) = go ys
  where
    go KF.Nil = False
    go zss@(KF.Fork x xc xs) = go xc || go xs || xss == fromJust (firstTree zss)

isChildOf :: Eq a => a -> KnuthTree a -> Bool
isChildOf x (KnuthTree (_,ys)) = KF.isChildOf x ys

isDescendantOf :: Eq a => a -> KnuthTree a -> Bool
isDescendantOf x (KnuthTree (y,ys)) = x == y || KF.isDescendantOf x ys

isProperDescendantOf :: Eq a => a -> KnuthTree a -> Bool
isProperDescendantOf x (KnuthTree (_,ys)) = KF.isDescendantOf x ys

-- ** Construction

singleton :: a -> KnuthTree a
singleton x = KnuthTree (x,KF.Nil)

delete :: Eq a => a -> KnuthTree a -> Maybe (KnuthTree a)
delete x (KnuthTree (y,ys)) | x == y = Nothing
                            | otherwise = Just $ KnuthTree (y, KF.delete x ys)

-- ** Combination

union :: KnuthTree a -> KnuthTree a -> KnuthTree a
union (KnuthTree (_,xs)) (KnuthTree (y,ys)) = KnuthTree (y, KF.union xs ys)

intersection :: Eq a => KnuthTree a -> KnuthTree a -> Maybe (KnuthTree a)
intersection (KnuthTree (x,xs)) (KnuthTree (y,ys)) = do
  guard $ x == y
  return $ KnuthTree (y,KF.intersection xs ys)

difference :: Eq a => KnuthTree a -> KnuthTree a -> Maybe (KnuthTree a)
difference xss@(KnuthTree (x,xs)) (KnuthTree (y,ys)) = do
  guard $ x /= y
  return $ KnuthTree (x,go ys)
  where
    go KF.Nil = KF.Nil
    go zss@(KF.Fork x xc xs) | xss == fromJust (firstTree zss) = KF.Nil
                             | otherwise = KF.Fork x (go xc) (go xs)
