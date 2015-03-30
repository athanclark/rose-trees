{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Tree.Rose where

import Data.Tree.Rose.Internal

import Data.Tree (Tree (Node))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Default
import Data.Functor.Identity

class RoseTree (c :: * -> *) (u :: * -> *) (t :: * -> *) | c -> u, c -> t where
  (@->) :: t a -> u (c a) -> c a

infixr 9 @->

instance RoseTree Tree [] Identity where
  (@->) (Identity x) xs = Node x xs

-- | Tagged rose tree where tags have a Default instance, where `def` is the
-- root node.
data PseudoTrie t a = More (t, Maybe a) (NonEmpty (PseudoTrie t a))
                    | Rest (NonEmpty t) a
                    | Nil
  deriving (Show, Eq)

newtype ContainerPT t a = ContainerPT (t, Maybe a)

instance Default t => RoseTree (PseudoTrie t) NonEmpty (ContainerPT t) where
  (@->) (ContainerPT x) xs = More x xs
