{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Tree.Knuth where

import Prelude hiding (foldr)
import Data.Monoid
import Data.Foldable

data KnuthForest a = Fork { node :: a
                          , children :: (KnuthForest a)
                          , siblings :: (KnuthForest a) }
                   | Nil
  deriving (Show, Eq, Functor)

appendSibling :: KnuthForest a -> KnuthForest a -> KnuthForest a
appendSibling Nil _ = Nil
appendSibling (Fork x xc Nil) y = Fork x xc y
appendSibling (Fork x xc xs) y = Fork x xc $ appendSibling xs y

instance Monoid (KnuthForest a) where
  mempty = Nil
  mappend = appendSibling

instance Foldable KnuthForest where
  foldr f acc Nil = acc
  foldr f acc (Fork x xc xs) =
    foldr f (foldr f (f x acc) xs) xc

newtype KnuthTree a = KnuthTree { unKnuthTree :: (a, KnuthForest a) }
  deriving (Show, Eq, Functor)

-- | Breadth-first
instance Foldable KnuthTree where
  foldr f acc (KnuthTree (x, xs)) = foldr f (f x acc) xs
