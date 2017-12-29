{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Set.RedBlackTree where

import Set.Class
import Ordered.Class

data Color = R | B

data RBTree a
  = E
  | T { color :: Color
      , left :: RBTree a
      , el :: a
      , right :: RBTree a
      }

instance Ordered a => Set RBTree a where
  -- empty :: RBTree a
  empty = E 

  -- insert :: a -> RBTree a -> RBTree a
  insert x s =
    let ins E                   = T R E x E
        ins s@(T color a y b)   = 
          if x `lt` y
             then balance (T color (ins a) y b)
             else
              if y `lt` x 
                 then balance (T color a y (ins b))
                 else s

        (T _ a y b) = ins s

    in T B a y b  

  -- member :: a -> RBTree a -> Bool
  member _ E                  = False
  member x (T _ left y right) 
    | x `lt` y                = member x left
    | y `lt` y                = member x right
    | otherwise               = True

balance :: RBTree a -> RBTree a
balance (T B (T R (T R a x b) y c) z d) = T R (T B a x b) y (T B c z d)  
balance (T B (T R a x (T R b y c)) z d) = T R (T B a x b) y (T B c z d)
balance (T B a x (T R (T R b y c) z d)) = T R (T B a x b) y (T B c z d)
balance (T B a x (T R b y (T R c z d))) = T R (T B a x b) y (T B c z d)
balance tree = tree
