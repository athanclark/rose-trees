{-# LANGUAGE
    FlexibleContexts
  #-}

module Data.Tree.KnuthBench (data_knuthtree_bench) where

import Prelude hiding (elem)
import Data.Monoid
import Data.Tree.Knuth
import qualified Data.Tree.Knuth.Forest as F
import Control.Monad.State
import Criterion


newNode :: MonadState Int m => F.KnuthForest Int -> m (KnuthTree Int)
newNode xs = do x <- get
                modify (+1)
                return $ KnuthTree (x, xs)

-- makeWith :: Int -> Int -> Int -> State Int (Tree Int)
makeWith d w r | d <= 1 || w <= 1 = return F.Nil
               | otherwise = do ws <- makeWith d (w-1) r
                                xs <- makeWith (d-1) (floor $ fromIntegral w / r) r
                                x <- get
                                modify (+1)
                                return $ F.Fork x xs ws

tree1 = evalState (makeWith 10 10 2 >>= newNode) 1
tree2 = evalState (makeWith 20 20 2 >>= newNode) 1
tree3 = evalState (makeWith 30 30 2 >>= newNode) 1
tree4 = evalState (makeWith 40 40 2 >>= newNode) 1
tree5 = evalState (makeWith 50 50 2 >>= newNode) 1


data_knuthtree_bench = bgroup "Data.Tree.Knuth"
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

maximum' :: KnuthTree Int -> Int
maximum' = maximum
