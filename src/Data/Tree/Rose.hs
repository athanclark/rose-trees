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


-- Data.Tree.Knuth
type instance Head (KnuthForest a) = a
type instance Tail (KnuthForest a) = KnuthForest a

instance RoseTree KnuthForest where
  x @-> xs = Fork x xs Nil
