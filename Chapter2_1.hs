module Chapter2_1 where

import Prelude hiding ((++))

-- Figure 2.1. Signature for stacks

class Stack s where
  empty   :: s a
  isEmpty :: s a -> Bool
  cons    :: a -> s a -> s a
  head    :: s a -> a
  tail    :: s a -> s a
  (++)    :: s a -> s a -> s a


-- Figure 2.2. Implementation of stacks
-- using the built-in type of lists

instance Stack [] where
  -- empty :: [a]
  empty = []

  -- isEmpty :: [a] -> Bool
  isEmpty [] = True

  -- cons :: a -> [a] -> [a]
  cons x xs = x:xs

  -- head :: [a] -> a
  head []     = error "EMPTY"
  head (x:xs) = x

  -- tail :: [a] -> [a]
  tail []     = error "EMPTY"
  tail (x:xs) = xs

  -- (++) :: [a] -> [a] -> [a]
  [] ++ ys      = ys
  (x:xs) ++ ys  = x:(xs ++ ys)

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
