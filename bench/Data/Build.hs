{-# LANGUAGE
    GADTs
  , FlexibleContexts
  #-}

module Data.Build where

import Data.Tree.Rose
import Control.Monad.State


newNode :: ( MonadState Int m
           , RoseTree t
           , Head (t Int) ~ Int
           ) => Tail (t Int)
             -> m (t Int)
newNode xs = do
  x <- get
  modify (+1)
  return $ x @-> xs

makeWith :: ( MonadState Int m
            , Head (t Int) ~ Int
            , RoseTree t
            ) => Int -- Depth
              -> Int -- Width
              -> Int -- Ratio
              -> Tail (t Int)
              -> ([t Int] -> Tail (t Int))
              -> m (t Int)
makeWith d w r empty fromList
  | d <= 1    = newNode empty
  | otherwise = do
      xs <- replicateM w $
              makeWith (d-1) (floor $ (fromIntegral w :: Float)
                                    / fromIntegral r) r empty fromList
      newNode $ fromList xs
