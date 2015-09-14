module Main where

import qualified Data.Tree
import           Data.TreeBench
import           Data.Tree.SetBench
import           Data.Tree.KnuthBench
import           Criterion.Main




main = defaultMain
  [ bgroup "Trees"
    [ data_tree_bench
    , data_settree_bench
    , data_knuthtree_bench
    ]
  ]
