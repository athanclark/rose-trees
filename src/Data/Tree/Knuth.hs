{-# LANGUAGE
    KindSignatures
  , DeriveFunctor
  , GeneralizedNewtypeDeriving
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

import Prelude hiding (foldr)
import Data.Monoid
import Data.Foldable
import Data.Traversable
import Control.Applicative
import Control.Monad


data KnuthForest a = Fork { kNode :: a
                          , kChildren :: KnuthForest a
                          , kSiblings :: KnuthForest a }
                   | Nil
  deriving (Show, Eq, Functor)

-- | Zipper-style
instance Applicative KnuthForest where
  pure x = Fork x Nil Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Fork f fc fs) <*> (Fork x xc xs) =
    Fork (f x) (fc <*> xc) (fs <*> xs)

-- | Breadth-first
instance Monad KnuthForest where
  return x = Fork x Nil Nil
  Nil >>= _ = Nil
  (Fork x xc xs) >>= f = f x `union` (xs >>= f) `union` (xc >>= f)

union :: KnuthForest a -> KnuthForest a -> KnuthForest a
union Nil _ = Nil
union (Fork x xc Nil) y = Fork x xc y
union (Fork x xc xs) y = Fork x xc $ union xs y

instance Monoid (KnuthForest a) where
  mempty = Nil
  mappend = union

-- | Breadth-first
instance Foldable KnuthForest where
  foldr _ acc Nil = acc
  foldr f acc (Fork x xc xs) =
    foldr f (foldr f (f x acc) xs) xc



newtype KnuthTree a = KnuthTree { unKnuthTree :: (a, KnuthForest a) }
  deriving (Show, Eq, Functor)

-- | Breadth-first
instance Foldable KnuthTree where
  foldr f acc (KnuthTree (x, xs)) = foldr f (f x acc) xs
