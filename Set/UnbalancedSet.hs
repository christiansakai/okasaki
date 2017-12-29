{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Set.UnbalancedSet where

import Set.Class
import Ordered.Class

data UnbalancedSet a 
  = E
  | T { left :: UnbalancedSet a
      , el :: a
      , right :: UnbalancedSet a
      }

instance Ordered a => Set UnbalancedSet a where
  -- empty :: UnbalancedSet a
  empty = E

  -- insert :: a -> UnbalancedSet a -> UnbalancedSet a
  insert x E            = T E x E
  insert x set@(T left y right)
    | x `lt` y          = insert x left
    | y `lt` x          = insert x right
    | otherwise         = set

   -- member :: a -> UnbalancedSet a -> Bool
  member _ E            = False
  member x (T left y right) 
    | x `lt` y          = member x left
    | y `lt` y          = member x right
    | otherwise         = True

