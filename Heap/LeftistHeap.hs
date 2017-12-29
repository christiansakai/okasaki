{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Heap.LeftistHeap where

import Ordered.Class
import Heap.Class

data LeftistHeap a
  = E
  | T { rank :: Int
      , el :: a
      , left :: LeftistHeap a
      , right :: LeftistHeap a
      }

instance Ordered a => Heap LeftistHeap a where
  -- empty :: LeftistHeap a
  empty = E

  -- isEmpty :: LeftistHeap a -> Bool
  isEmpty E = True
  isEmpty _ = False

  -- insert :: a -> LeftistHeap a -> LeftistHeap a
  insert x h = merge (T 1 x E E) h

  -- merge :: LeftistHeap a -> LeftistHeap a -> LeftistHeap a
  merge h E = h
  merge E h = h
  merge h1@(T _ x h1l h1r) 
        h2@(T _ y h2l h2r) = 
          if x `leq` y
             then makeHeap x h1l (merge h1r h2)
             else makeHeap y h2l (merge h1 h2r)

  -- findMin   :: LeftistHeap a -> a
  findMin E           = error "EMPTY"
  findMin (T _ x _ _) = x

  -- deleteMin :: LeftistHeap a -> LeftistHeap a
  deleteMin E           = error "EMPTY"
  deleteMin (T _ _ l r) = merge l r

-- Helper function that calculates
-- the rank of a T node and swaps its
-- children if necessary
makeHeap :: a 
         -> LeftistHeap a 
         -> LeftistHeap a
         -> LeftistHeap a
makeHeap x hl hr =
  if rank hl >= rank hr
     then T (rank hr + 1) x hl hr
     else T (rank hl + 1) x hr hl
