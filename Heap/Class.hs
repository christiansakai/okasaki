{-# LANGUAGE MultiParamTypeClasses #-}

module Heap.Class where

import Ordered.Class

class Ordered a => Heap h a where
  empty     :: h a
  isEmpty   :: h a -> Bool
  insert    :: a -> h a -> h a
  merge     :: h a -> h a -> h a
  findMin   :: h a -> a
  deleteMin :: h a -> h a



