{-# LANGUAGE
    TypeFamilies
  , KindSignatures
  , MultiParamTypeClasses
  , FunctionalDependencies
  , FlexibleInstances
  #-}

module Data.Tree.Rose where

import Data.Tree (Tree (Node))
import Data.Tree.Knuth
import Data.Tree.Knuth.Forest as KF
import Data.Tree.Set
import Data.Tree.Hash
import qualified Data.Set as Set
import qualified Data.HashSet as HS


type family Head (x :: *) :: *
type family Tail (y :: *) :: *

class RoseTree (c :: * -> *) where
  (@->) :: Head (c a) -> Tail (c a) -> c a

infixr 9 @->


-- Data.Tree
type instance Head (Tree a) = a
type instance Tail (Tree a) = [Tree a]

instance RoseTree Tree where
  (@->) = Node


-- Data.Tree.Knuth.Forest
type instance Head (KnuthForest a) = a
type instance Tail (KnuthForest a) = KnuthForest a

instance RoseTree KnuthForest where
  x @-> xs = Fork x xs Nil


-- Data.Tree.Knuth
type instance Head (KnuthTree a) = a
type instance Tail (KnuthTree a) = KnuthForest a

instance RoseTree KnuthTree where
  x @-> xs = KnuthTree (x,xs)


-- Data.Tree.Set
type instance Head (SetTree a) = a
type instance Tail (SetTree a) = Set.Set (SetTree a)

instance RoseTree SetTree where
  (@->) = SetTree


-- Data.Tree.Hash
type instance Head (HashTree a) = a
type instance Tail (HashTree a) = HS.HashSet (HashTree a)

instance RoseTree HashTree where
  (@->) = HashTree
