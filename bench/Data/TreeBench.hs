{-# LANGUAGE
    FlexibleContexts
  #-}

module Data.TreeBench (data_tree_bench) where

import Data.Monoid
import Data.Tree
import Data.Tree.Rose
import Control.Monad.State
import Criterion


newNode :: MonadState Int m => [Tree Int] -> m (Tree Int)
newNode xs = do x <- get
                modify (+1)
                return $ Node x xs

-- makeWith :: Int -> Int -> Int -> State Int (Tree Int)
makeWith d w r | d <= 1 = newNode []
               | otherwise = do xs <- replicateM w $ makeWith (d-1) (floor $ (fromIntegral w) / r) r
                                newNode xs

tree1 = evalState (makeWith 1 1 2) 1
tree2 = evalState (makeWith 2 2 2) 1
tree3 = evalState (makeWith 3 3 2) 1
tree4 = evalState (makeWith 4 4 2) 1
tree5 = evalState (makeWith 5 5 2) 1


data_tree_bench = bgroup "Data.Tree"
  [ bench "1" $ whnf (elemT 1) tree1
  , bench "2" $ whnf (elemT 2) tree2
  , bench "3" $ whnf (elemT 5) tree3
  , bench "4" $ whnf (elemT 18) tree4
  , bench "5" $ whnf (elemT 23) tree5
  ]

elemT :: Eq a => a -> Tree a -> Bool
elemT x (Node y ys) = x == y || getAny (foldMap (Any . elemT x) ys)

sizeT :: Tree a -> Int
sizeT (Node _ xs) = 1 + getSum (foldMap (Sum . sizeT) xs)
