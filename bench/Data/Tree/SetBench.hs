{-# LANGUAGE
    FlexibleContexts
  #-}

module Data.Tree.SetBench (data_settree_bench) where

import Data.Build
import Prelude hiding (elem)
import Data.Monoid
import qualified Data.Set as Set
import Data.Tree.Set
import Control.Monad.State
import Criterion



tree1 = evalState (makeWith 10 10 2 Set.empty Set.fromList) 1
tree2 = evalState (makeWith 20 20 2 Set.empty Set.fromList) 1
tree3 = evalState (makeWith 30 30 2 Set.empty Set.fromList) 1
tree4 = evalState (makeWith 40 40 2 Set.empty Set.fromList) 1
tree5 = evalState (makeWith 50 50 2 Set.empty Set.fromList) 1


data_settree_bench = bgroup "Data.Tree.Set"
  [ bgroup "depth"
    [ bench "1" $ whnf maximum' tree1
    , bench "2" $ whnf maximum' tree2
    , bench "3" $ whnf maximum' tree3
    , bench "4" $ whnf maximum' tree4
    , bench "5" $ whnf maximum' tree5
    ]
  , bgroup "width"
    [ bench "1" $ whnf maximum' tree1
    , bench "2" $ whnf maximum' tree2
    , bench "3" $ whnf maximum' tree3
    , bench "4" $ whnf maximum' tree4
    , bench "5" $ whnf maximum' tree5
    ]
  ]

maximum' :: SetTree Int -> Int
maximum' = maximum
