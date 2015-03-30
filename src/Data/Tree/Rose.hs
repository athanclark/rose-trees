{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Tree.Rose where

import Data.Tree.Rose.Internal


class RoseTree (c :: * -> *) (u :: * -> *) | c -> u where
  (@->) :: a -> u a -> c a
