module Ordered.Class where

class Ordered a where
  eq    :: a -> a -> Bool
  lt    :: a -> a -> Bool
  leq   :: a -> a -> Bool

