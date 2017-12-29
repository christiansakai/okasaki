module Stack.CustomStack where

import Stack.Class
import Prelude hiding ((++))

-- Figure 2.3. Implementation of stacks
-- using a custom data type

data CustomStack a 
  = Nil
  | Cons a (CustomStack a)
  deriving Show

instance Stack CustomStack where
  -- empty :: CustomStack a
  empty = Nil

  -- isEmpty :: CustomStack a -> Bool
  isEmpty Nil = True
  isEmpty _   = False

  -- cons :: a -> CustomStack a -> CustomStack a
  cons x xs = Cons x xs

  -- head :: CustomStack a -> a
  head Nil        = error "EMPTY"
  head (Cons x _) = x

  -- tail :: [a] -> [a]
  tail Nil          = error "EMPTY"
  tail (Cons _ xs)  = xs

  -- (++) :: CustomStack a -> CustomStack a -> CustomStack a
  Nil ++ ys         = ys
  (Cons x xs) ++ ys = Cons x (xs ++ ys)

  -- update  :: s a -> Int -> a -> s a
  update Nil _ _            = error "SUBSCRIPT" 
  update (Cons x xs) 0 y    = Cons y xs 
  update (Cons x xs) pos y  = update xs (pos - 1) y

  -- suffixes :: s a -> s (s a)
  suffixes Nil                = Cons Nil Nil
  suffixes stack@(Cons x xs)  = Cons stack (suffixes xs)
