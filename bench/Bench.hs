module Main where

import qualified Data.Tree
import           Data.TreeBench
import           Criterion.Main




main = defaultMain
  [ bgroup "Trees"
    [ data_tree_bench
    ]
  ]
