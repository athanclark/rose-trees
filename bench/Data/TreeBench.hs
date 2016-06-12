{-# LANGUAGE
    FlexibleContexts
  #-}

module Data.TreeBench (data_tree_bench) where

import Data.Build
import Data.Monoid
import Data.Tree
import Data.Tree.Rose
import Control.Monad.State
import Criterion


tree1 = evalState (makeWith 10 10 2 [] id) 1
tree2 = evalState (makeWith 20 20 2 [] id) 1
tree3 = evalState (makeWith 30 30 2 [] id) 1
tree4 = evalState (makeWith 40 40 2 [] id) 1
tree5 = evalState (makeWith 50 50 2 [] id) 1


data_tree_bench = bgroup "Data.Tree"
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

maximum' :: Tree Int -> Int
maximum' = maximum
