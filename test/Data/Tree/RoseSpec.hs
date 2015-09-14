module Data.Tree.RoseSpec where

import Data.Tree.Rose

import Data.Monoid
import qualified Data.Tree as T
import qualified Data.Set as Set
import qualified Data.Tree.Set as S
import qualified Data.Tree.Knuth as K
import qualified Data.Tree.Knuth.Forest as KF


import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck
import Test.QuickCheck.Instances


spec :: TestTree
spec = testGroup "Testing trees..."
  [ testGroup "Data.Tree"
    [ QC.testProperty "element after construction" prop_cons_exists_Tree
    ]
  , testGroup "Data.Tree.Set"
    [ QC.testProperty "element after construction" prop_cons_exists_SetTree
    ]
  , testGroup "Data.Tree.Knuth"
    [ QC.testProperty "element after construction" prop_cons_exists_KnuthTree
    ]
  ]

newtype TreeWithElem t e = TreeWithElem {unTreeWithElem :: (e, t e)}


prop_cons_exists_Tree :: Int -> [T.Tree Int] -> Bool
prop_cons_exists_Tree x xs = elem' x $ x @-> xs
  where elem' x (T.Node y ys) = x == y || getAny (foldMap (Any . elem' x) ys)

prop_cons_exists_SetTree :: Int -> Set.Set (S.SetTree Int) -> Bool
prop_cons_exists_SetTree x xs = S.elem x $ x @-> xs

prop_cons_exists_KnuthTree :: Int -> KF.KnuthForest Int -> Bool
prop_cons_exists_KnuthTree x xs = K.elem x $ x @-> xs


-- instance (Arbitrary e
