{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Heap.BinomialHeap where

import Ordered.Class
import Heap.Class

data BnmTree a = 
  Node { rank :: Int
       , el :: a
       , list :: [BnmTree a]
       }

newtype BnmHeap a = BnmHeap { unBnmHeap :: [BnmTree a] }
             
-- Binomial Heap is a list of Binomial Trees on increasing order of rank
instance Ordered a => Heap BnmHeap a where
  -- empty :: BnmHeap a
  empty = BnmHeap []

  -- isEmpty :: BnmHeap a -> Bool
  isEmpty (BnmHeap []) = True
  isEmpty (BnmHeap _)  = False

  -- insert :: a -> BnmHeap a -> BnmHeap a
  insert x heap = insertTree (Node 0 x []) heap

  -- merge :: BnmHeap a -> BnmHeap a -> BnmHeap a
  merge (BnmHeap []) heap = heap
  merge heap (BnmHeap []) = heap
  merge heap1 heap2 =
    let (t1:ts1) = unBnmHeap heap1
        (t2:ts2) = unBnmHeap heap2
     in if rank t1 < rank t2
           then BnmHeap (t1:(unBnmHeap (merge (BnmHeap ts1) heap2)))
           else 
            if rank t2 < rank t1
               then BnmHeap (t2:(unBnmHeap (merge heap1 (BnmHeap ts2))))
               else insertTree (link t1 t2) (merge (BnmHeap ts1) (BnmHeap ts2))

--   -- findMin :: BnmHeap a -> a
  findMin heap = let (tree, _) = removeMinTree heap
                  in el tree

  -- deleteMin :: BnmHeap a -> BnmHeap a
  deleteMin heap =
    let (Node _ el ts1, ts2) = removeMinTree heap
     in merge (BnmHeap . reverse $ ts1) ts2

insertTree :: Ordered a => BnmTree a -> BnmHeap a -> BnmHeap a
insertTree t (BnmHeap [])                = BnmHeap [t]
insertTree t (BnmHeap treeList@(t':ts')) =
  if rank t < rank t'
     then BnmHeap (t:treeList)
     else insertTree (link t t') (BnmHeap ts')

-- Link trees with larger roots
-- under trees with smaller roots to
-- maintain heap order
link :: Ordered a => BnmTree a -> BnmTree a -> BnmTree a
link t1@(Node r1 e1 ts1) t2@(Node _r2 e2 ts2) =
  if e1 `leq` e2
     then Node (r1 + 1) e1 (t2:ts1)
     else Node (r1 + 1) e2 (t1:ts2)

-- Removes the tree with the minimum root 
-- and removes it from the list
removeMinTree :: Ordered a => BnmHeap a -> (BnmTree a, BnmHeap a)
removeMinTree (BnmHeap [t])    = (t, BnmHeap [])
removeMinTree (BnmHeap (t:ts)) =
  let (tree, heap) = removeMinTree (BnmHeap ts)
   in if el t `leq` el tree
         then (t, BnmHeap ts) 
         else (tree, BnmHeap (t:(unBnmHeap heap)))
