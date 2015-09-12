module Main where

import qualified Data.Trie.Knuth as K
import           Criterion.Main




main = defaultMain
  [ bgroup "Lit vs. Pred"
    [ bgroup "Lit"
      [ bench "1" $ whnf (U.lookup ["1"]) doubleLit
      , bench "2" $ whnf (U.lookup ["2"]) doubleLit
      , bench "3" $ whnf (U.lookup ["3"]) doubleLit
      , bench "4" $ whnf (U.lookup ["4"]) doubleLit
      ]
    ]
  ]
