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

tree1 = evalState (makeWith 1 1 2 >>= newNode) 1
tree2 = evalState (makeWith 2 2 2 >>= newNode) 1
tree3 = evalState (makeWith 3 3 2 >>= newNode) 1
tree4 = evalState (makeWith 4 4 2 >>= newNode) 1
tree5 = evalState (makeWith 5 5 2 >>= newNode) 1


data_knuthtree_bench = bgroup "Data.Tree.Knuth"
  [ bgroup "depth"
    [ bench "1" $ whnf (elem 1) tree1
    , bench "2" $ whnf (elem 2) tree2
    , bench "3" $ whnf (elem 5) tree3
    , bench "4" $ whnf (elem 18) tree4
    , bench "5" $ whnf (elem 23) tree5
    ]
  , bgroup "width"
    [ bench "1" $ whnf (elem 1) tree1
    , bench "2" $ whnf (elem 2) tree2
    , bench "3" $ whnf (elem 6) tree3
    , bench "4" $ whnf (elem 20) tree4
    , bench "5" $ whnf (elem 25) tree5
    ]
  ]
